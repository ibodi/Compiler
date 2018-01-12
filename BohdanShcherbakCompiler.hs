{-# LANGUAGE Safe #-}

{-
To jest moja wersja kompilatora dla procesora Motorola 6809 języka programowania do którego 
pisaliśmy interpreter robiąc pracownię szóstą. 

Kompilator ten tłumaczy nasz prosty język programowania na makroasembler, tworząc listę 
instrukcji makroasemblera, które w haskelu są reprezentowane przez typ MInstr zdefiniowany 
w module MacroAsm.

Nie patrząc na to, że w rozważanym języku funkcje są traktowane tak samo jak zmienne i 
znajdują się w tym samym środowisku, w przeznaczonej dla kompilowania funkcji comp 
używam dla wygody osobnej zmiennej dla trzymania informacji o funkcjach globalnych 
(czyli etykiet tych funkcji). Bo żeby znaleźć wartość zmiennej potrzebujemy znać 
gdzie ona się znajduje na stosie, a żeby wywołać potrzebną funkcję globalną wystarczy 
znać jej etykietę.

Używam monady State przy generowaniu listy instrukcji, bez której, pewnie, dało by się 
obejść, bo pozwala ona wygodnie generować nowe etykiety niezbędne dla wykonania skoków i 
procedur tak, żeby one się nie powtarzały.

Ponieważ język asemblerowy jest najbardziej czytelny gdy każda instrukcja jest w nowym
wierszu, listy instrukcji starałem się pisać w podobnym stylu.

Przyjąłem taką zasadę, że każde obliczenie musi zostawić stan stosu takim jak był on 
do tego obliczenia, a wyniki obliczeń są umieszczane w akumulatorze.
-}

module BohdanShcherbakCompiler(compile) where
import AST
import MacroAsm
import Control.Monad.State

-- Typ przeznaczony dla trzymania par nazw zmiennych oraz liczb odpowiadających pozycji tych zmiennych 
-- na stosie licząc od dołu stosu lub od dołu pewnej górnej części stosu.
type Env 	= [(Var, Int)] 

-- Typ przeznaczony dla trzymania par nazw funkcji globalnych oraz etykiet procedur odpowiadających 
-- tym funkcjom w rozważanym języku.
type FLbls 	= [(Var, Label)]

-- Funkcja dla generowania nowej etykiety w monadzie State.
newLabel:: State Label Label
newLabel = do
	l <- get
	put (l+1)
	return l

-- Funkcja dla przetwarzania listy z nazwami zmiennych na listę, która zawiera pary 
-- nazw zmiennych i odpowiadających pozycji tych zmiennych na stosie licząc od dołu stosu
-- w momencie przed rozpoczęciem działania programu i jednocześnie pozwala otrzymać 
-- pozycję zmiennej na górze stosu.
varsToEnv:: [Var] -> (Int, Env)
varsToEnv [] = (-1, [])
varsToEnv (v:vs) = 
	(n+1, (v, n+1):xs)
	where (n, xs) = varsToEnv vs

-- Funkcja pozwalająca otrzymać listę par nazw funkcji i ich przyszłych etykiet oraz 
-- liczbę tych funkcji.
funToFEnv:: [FunctionDef p] -> Label -> (Int, FLbls)
funToFEnv [] _ = (0, [])
funToFEnv (f:fs) n = 
	(fsLen+1, (funcName f, n):fsenv) 
	where 
		(fsLen, fsenv) = funToFEnv fs (n+1)

-- Funkcja pozwalająca przetworzyć listę funkcji na listę instrukcji procedur żeby ją można 
-- było dokleić na konieć do kodu programu. Drugi argument odpowiada etykiecie procedury 
-- odpowiadającej funkcji, która jest głową listy będącej pierwszym argumentem.
funToProc:: [FunctionDef p] -> Label -> FLbls -> State Label [MInstr]
funToProc [] _ _ = do
	return []
funToProc (f:fs) n flbls = do
	fIns <- comp ((funcArg f, 0):[]) flbls 1 (funcBody f)
	fsIns <- funToProc fs (n+1) flbls
	return ([MLabel n] ++ 
			fIns ++ 
			[MRet] ++ 
			fsIns)

-- Funkcja pozwalająca otrzymać listę instrucji odpowiadających pewnemu operatorowi binarnemu.
operToInst:: BinaryOperator -> State Label [MInstr]
operToInst op = do
	case op of
		BAnd -> return [MAnd]
		BOr  -> return [MOr]
		BNeq -> return [MNeg]
		BAdd -> return [MAdd]
		BSub -> return [MSub]
		BMul -> return [MMul]
		BDiv -> return [MDiv]
		BMod -> return [MMod]
		compOp -> do
			l1 <- newLabel
			l2 <- newLabel
			return [MBranch (compOpToCond compOp) l1, 
					MConst 0, 
					MJump l2, 
					MLabel l1, 
					MConst (-1), 
					MLabel l2]
		where 
			compOpToCond:: BinaryOperator -> MCondition
			compOpToCond compOp = case compOp of
				BEq  -> MC_EQ
				BLt  -> MC_LT
				BLe  -> MC_LE
				BGt  -> MC_GT
				BGe  -> MC_GE

-- Jasne co robi
compile :: [FunctionDef p] -> [Var] -> Expr p -> [MInstr]
compile fs vs e = (fst (runState (comp env flbls stHt e) lbl)) ++ [MRet] ++ fsIns
	where
		(stHt, env) = varsToEnv vs
		(fsLen, flbls) = funToFEnv fs 0
		(fsIns, lbl) = runState (funToProc fs 0 flbls) fsLen
 
-- comp to jest funkcja, która pełni główną rolę.
-- Jej "czwarty argument" (e::Expr p) to wyrażenie jakie chcemy skompilować
-- Pierwszy (env::Env) to lista par nazw zmiennych widocznych dla danego wyrażenia i ich wysokości 
-- w części stosu liczącej (stHt+1) zmiennych na górze stosu. 
-- Drugi (flbls::Flbls) to lista par nazw funkcji globalnych i ich etykiet.
comp:: Env -> FLbls -> Label -> Expr p -> State Label [MInstr]
-- Ponieważ trochę odróżniam funkcje globalne i zmienne przy kompilowaniu,
-- ale traktowane one muszą być tak samo, birąc do uwagi fakt, że nazwy zmiennych
-- wejściowych muszą zakrywać nazwy funcji w razie kolizji nazw (bo są one w jednym środowisku), w momencie 
-- szukania zmiennej najpierw szukam ją w env czyli spośród zmiennych bez funkcji globalnych, obliczam w jakim 
-- miejscu na stosie zmienna się zmajduję i załadowuję ją do akumulatora. A jeśli nie znajduję,
-- znaczy jest ta zmienna funkcją globalną i szukam jej etykietę w lblbs.
-- Funkcje globalne są reprezentowane w makroasemblerze przez 2 bajty na stercie 
-- z adresem etykiety odpowiedniej procedury.
comp env flbls stHt (EVar _ var) = 
	case lookup var env of
		Just n 	-> return  [MGetLocal (stHt-n)]
		Nothing	-> return  [MAlloc 1, 
							MPush, 
							MGetLabel (myLookup var flbls), 
							MSet 0, 
							MPopAcc]
	where
		myLookup::Eq a=>a-> [(a, b)] -> b
		myLookup var env = case lookup var env of {Just x -> x; Nothing -> undefined}
comp env flbls stHt (ENum _ num) = do -- "do" jest dla czytelności
	return [MConst num]
-- W makroasemblerze znaczenie prawdy jest reprezentowane przez liczbę (-1), a fałszu 
-- przez liczbę 0, bo jedną można otrzymać z drugiej przez wykonanie instrukcji MNot 
-- (negacja bitowa).
comp env flbls stHt (EBool _ bool) = do
	return [MConst intBool]
	where 
		intBool = if bool then (-1) else 0
comp env flbls stHt (EUnary _ op e) = do
	eIns <- comp env flbls stHt e
	return (eIns ++ 
			case op of
				UNeg -> [MNeg]
				UNot -> [MNot])
-- Żeby wykonać operację binarną w asemblerze obliczmy lewy argument ummieszczamy go na stosie
-- obliczamy prawy argument i wykonujemy operację.
comp env flbls stHt (EBinary _ op e1 e2) = do
	e1Ins <- comp env flbls stHt e1
	e2Ins <- comp env flbls (stHt+1) e2
	opIns <- operToInst op
	return (e1Ins ++ 
			[MPush] ++ 
			e2Ins ++ 
			opIns)
-- Kiedy dodajemy zmienną do środowiska przy pomocy let obliczamy jej znaczenie, umieszczamy na
-- stosie, po czym obliczamy główne wyrażenie. Nie możemy także zapomnieć o usunięciu 
-- wartości zmiennej ze stosu.
comp env flbls stHt (ELet _ var e1 e2) = do
	e1Ins <- comp env flbls stHt e1
	e2Ins <- comp ((var, stHt + 1):env) flbls (stHt+1) e2
	return (e1Ins ++ 	-- obliczam wartośc wyrażenia
			[MPush] ++ 	-- umieszczam ją na stosie
			e2Ins ++ 	
			[MPopN 1])	-- sprzątam
comp env flbls stHt (EIf _ eCond eThen eElse) = do -- jasne
	l1 <- newLabel
	l2 <- newLabel
	condIns <- comp env flbls stHt eCond
	thenIns <- comp env flbls stHt eThen
	elseIns <- comp env flbls stHt eElse
	return (condIns ++ 
			[MBranch MC_Z l1] ++ 
			thenIns ++ 
			[MJump l2, 
			MLabel l1] ++ 
			elseIns ++ 
			[MLabel l2])
-- Przy założeniu, że kompilowane programy są dobrze typowane, jedyną wartość typu unit
-- wygodnie reprezentować przez 0.
comp env flbls stHt (EUnit _) = do
	return [MConst 0]
-- Obliczenie pary, fst i snd są oczywiste.
comp env flbls stHt (EPair _ el er) = do
	elIns <- comp env flbls (stHt+1) el
	erIns <- comp env flbls (stHt+1) er
	return ([MAlloc 2, 
			MPush] ++ 
			elIns ++ 
			[MSet 0] ++ 
			erIns ++ 
			[MSet 1, 
			MPopAcc])
comp env flbls stHt (EFst _ e) = do 
	eIns <- comp env flbls stHt e
	return (eIns ++ 
			[MGet 0])
comp env flbls stHt (ESnd _ e) = do
	eIns <- comp env flbls stHt e
	return (eIns ++ 
			[MGet 1])
-- Lista pusta jest też reprezentowana przez 0.
comp env flbls stHt (ENil _ _) = do
	return [MConst 0]
-- Jak widać lista niepusta się oblicza dokładnie tak samo jak i para.
-- To jest wygodne, bo z łatwością potem będziemy mogli "odrywać głowy"
-- listom, czyli z nimi pracować.
comp env flbls stHt (ECons _ x xs) = do
	xIns <- comp env flbls (stHt+1) x
	xsIns <- comp env flbls (stHt+1) xs
	return ([MAlloc 2, 
			MPush] ++ 
			xIns ++ 
			[MSet 0] ++ 
			xsIns ++ 
			[MSet 1, 
			MPopAcc])
comp env flbls stHt (EMatchL _ e nilCase (hvar, tvar, expr)) = do
	l1 <- newLabel
	l2 <- newLabel
	eIns <- comp env flbls stHt e
	nilCaseIns <- comp env flbls stHt nilCase
	exprIns <- comp ((hvar, stHt+3):(tvar, stHt+2):env) flbls (stHt + 3) expr
	return (eIns ++ 
			[MBranch MC_Z l1,
			MPush, 		-- jeśli lista nie jest pustą, umieszczam ją na stosie 
			MGet 1, 
			MPush, 		-- umieszczam jej ogon na stosie
			MGetLocal 1, 
			MGet 0, 	
			MPush] ++ 	-- umieszczam jej głowę na stosie
			exprIns ++ 	-- obliczamy wyrażenie
			[MPopN 3, 	-- po czym zdejmujemy ze stosu listę, jej głowę i ogon (sprzątam)
			MJump l2, 	-- przeskakujemy
			MLabel l1] ++ 
			nilCaseIns ++ -- jeśli lista jest pustą
			[MLabel l2])
-- Funkcje globalne, jak było powiedziane, są reprezentowane w makroasemblerze przez 2 
-- bajty na stercie z adresem etykiety odpowiedniej procedury. One są tak reprezentowane 
-- jedynie z powodu konieczności jednakowego traktowania lambd i funkcji globalnych.
-- Lambdy są reprezentowane przez 2*(n+1) bajtów na stercie, na której jest umieszczony adres
-- etykiety odpowiedniej procedury oraz wszystkie wartości zmiennych widocznych dla tej 
-- lambdy w momencie jej utworzenia, gdzie n jest liczbą tych zmiennych.
comp env flbls stHt (EApp _ funcE argE) = do
	argEIns <- comp env flbls stHt argE
	funcEIns <- comp env flbls (stHt+1) funcE
	return (argEIns ++ 	-- umieszczamy na stosie argument funkcji
			[MPush] ++ 
			funcEIns ++ -- obliczamy obiczamy adres miejsca na stercie z danymi o procedurze (adres)
			[MPush, 
			MGet 0, 	-- wyjmujemy jej etykietę
			MCallAcc, 	-- wywołujemy odpowiednią procedurę
			MPopN 2])	-- nie zapominamy usunąć ze stozu adres i argument
-- Z lambdami jest najwięcej problemów :/. Przy utworzeniu lambdy, kod odpowiadającej jej 
-- procedury wklejamy wprost. Wywoływać procedurę co prawda jescze nie potrzebujemy, dlatego
-- korzystamy z instrukcji skoku MJump. 
comp env flbls stHt (EFn _ arg _ e) = do
	lAfter <- newLabel
	l <- newLabel
	eIns <- comp ((arg, 0):envVarsPlus2 env) flbls (stHt+2) e -- zmieniamy wysokość stosu o 2,
														-- bo chcemy "widzieć" argument lambdy
	return ([MJump lAfter, 	-- przeskakujemy kod lambdy 
			MLabel l] ++ 	-- etykieta lambdy
			getStosFromAlloc (stHt + 1) 0 ++ -- zdejmujemy ze sterty wartości zmiennych widocznych
											-- w momencie utworzenia lambdy
			eIns ++ 		-- wykonujemy kod lambdy
			[MPopN (stHt+1),-- sprzątamy
			MRet, 
			MLabel lAfter, 	
			MAlloc (stHt+2),-- Rezerwujemy miejsce na stercie
			MPush] ++ 
			putStosToAlloc (stHt+1) ++ -- umieszczamy wszystkie widoczne zmienne na stercie
			[MGetLabel l, 	
			MSet 0, 		-- umieszczamy adres etykiety na stercie
			MPopAcc]) 		
	where
		-- Funkcja, która utrarza listę instrukcji umieszczających wszystkie widoczne 
		-- dla lambdy zmienne na stercie
		putStosToAlloc::Int->[MInstr]
		putStosToAlloc n = 
			case n of
				0 	-> [] 
				n 	-> [MGetLocal n, 
						MSet n] ++ 
						putStosToAlloc (n-1)
		-- Funkcja, która utrarza listę instrukcji zdejmujących ze sterty wartości 
		-- zmiennych widocznych w momencie utworzenia lambdy
		getStosFromAlloc::Int->Int->[MInstr]
		getStosFromAlloc n hgt = -- hgt to jest glębokość na której się znajduję adres miejsca 
								-- na stercie z etykietą i wartościami zmiennych (AMSEWZ)
			case n of
				0 	-> []
				n 	-> [MGetLocal hgt, 	-- dostajemy AMSEWZ
						MGet n, 		-- wyjmujemy ze sterty n-tą zmienną
						MPush] ++ 		-- umieszczamy na stosie
						getStosFromAlloc (n-1) (hgt+1)
		-- Ponieważ dla lambdy musi być widoczny jej argument zwiększamy znaczenia wysokości 
		-- zmiennych nad dołem górnej części stosu widocznej dla lambdy w momencie wykonania 
		-- odpowiedniej procedury o 2. Po czym argument lambdy będzie na zerowym miejscu.
		envVarsPlus2::Env->Env
		envVarsPlus2 env =
			case env of
				[] 	-> []
				(var, n):vs -> (var, n+2): envVarsPlus2 vs

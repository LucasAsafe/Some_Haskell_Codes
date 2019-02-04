module Main where
import Control.Concurrent
import Control.Concurrent.MVar


waitThreads :: MVar Int -> IO ()
waitThreads fim = 
  do f <- takeMVar fim
     if (f > 0) then
         do putMVar fim f
            waitThreads fim
     else 
         return ()

fabricaParafusos :: MVar Int -> MVar Int ->  IO ()
fabricaParafusos paraf fim 
    = do 
	      f <- takeMVar fim    
	      p <- takeMVar paraf
	      putStrLn ("fParaf - num paraf: " ++ show(p+1))
	      putStrLn ("fParaf - fim: " ++ show(f-1))       
	      putMVar paraf (p + 1)
	      putMVar fim (f-1)
	      fabricaParafusos paraf fim    

      
fabricaPorcas :: MVar Int -> MVar Int ->  IO ()
fabricaPorcas porcas fim 
 = do 
      p <- takeMVar porcas
      f <- takeMVar fim
      --putMVar porcas (p + 1)    
      putStrLn ("fPorca - valor de fim: " ++ show(f-1))
      putStrLn ("fPorca - num inicial de porcas: "++ show(p))
      putStrLn ("fPorca - num de final de porcas: " ++ show(p+1))
      putMVar fim (f-1)
      putMVar porcas (p + 1)
      fabricaPorcas porcas fim    

montador:: String -> MVar Int -> MVar Int -> MVar Int -> MVar Int -> IO ()
montador id porcas paraf pares  fim
 = do
     pr <- takeMVar pares
     pc <- takeMVar porcas
     pf <- takeMVar paraf
     f  <- takeMVar fim
     if pc > 1 && pf > 1
     then do 
     	     putStrLn(id ++ " pares")             
             putStrLn(id ++ " porcas")             
             putStrLn(id ++ " parafusos")             
             putStrLn (id ++ "pares: " ++ show(pr+1))            
             putStrLn(id ++ ": valor de fim:" ++ show(f-1))
             putStrLn(id ++ ": terminando mais um par")
             putMVar pares (pr + 1)
             putMVar paraf (pf - 1)
             putMVar porcas (pc - 1)
             putMVar fim (f-1)
             montador id porcas paraf pares fim 
     else
          do putMVar pares pr
             putMVar paraf pf
             putMVar porcas pc
             putMVar fim f
             montador id porcas paraf pares fim 
       
main :: IO ()
main = do 
	fim1 <- newMVar 8
	fim2 <- newMVar 4
--	fim3 <- newMVar 25
--	fim4 <- newMVar 25
        porcas <- newMVar  0 
        parafusos <- newMVar  0
        pares <- newMVar 0  
        mut <- newMVar 0
        forkIO (fabricaParafusos parafusos fim1)
        forkIO (fabricaPorcas porcas fim1)
        forkIO (montador "maq1" porcas parafusos pares fim2)
        forkIO (montador "maq2" porcas parafusos pares fim2)   
        waitThreads fim1         
        waitThreads fim2
--        waitThreads fim3         
--        waitThreads fim4

        pr <- takeMVar pares
        putMVar pares pr
        pc <- takeMVar porcas
        putMVar porcas pc
        pf <- takeMVar parafusos   
        putMVar parafusos pf
        putStrLn ("porcas: " ++ show(pc))
        putStrLn ("parafusos: " ++ show(pf))
        putStrLn ("pares: " ++ show(pr))
        return ()
          


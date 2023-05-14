{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module VendingMachine (
  insertCoin, selectProduct, returnCoins, Product (..), Instruction (..), Coin (..), Time (..), VendingMachine (..)
) where

import Control.Monad.State
import Text.Printf
import qualified Data.List as List

data Instruction =
  AcceptCoin |
  RejectCoin |
  Display String |
  CollectCurrentAmount |
  ReturnCurrentAmount |
  DisplayTimed String Time |
  Dispense String |
  Return [Coin] deriving (Eq, Show)

data Time = Second Integer deriving (Eq, Show)

data Coin = Penny | Nickel | Dime | Quarter deriving (Eq, Show)

data VendingMachine = VendingMachine { currentAmount :: [Coin], stock :: [Product], cash :: [Coin] } deriving (Eq, Show)

data Product = Product { key :: String, price :: Float, quantity :: Integer } deriving (Eq, Show)

insertCoin :: Coin -> State VendingMachine [Instruction]
insertCoin coin = state $ \vm ->
  let newAmount = coin : currentAmount vm
      newState = vm { currentAmount = newAmount }
      valid Penny = False; valid _ = True
  in if valid coin then ( [AcceptCoin, display newState], newState ) else ( [RejectCoin, display vm], vm)

selectProduct :: String -> State VendingMachine [Instruction]
selectProduct code = state $ \vm ->
  case List.find (\p -> key p == code) (stock vm) of
    Nothing -> ([Display "INVALID PRODUCT"], vm)
    Just p -> dispense p vm

dispense :: Product -> VendingMachine -> ([Instruction], VendingMachine)
dispense p vm
  | not enoughStock =
    ([DisplayTimed "SOLD OUT" (Second 5), display vm ], vm )
  | not enoughMoney =
    ([DisplayTimed ("PRICE "++ ((printf "$%.2f" . (/100)) (price p))) (Second 5), display vm], vm)
  | tooMuchMoney && not enoughChange =
    ([Display "EXACT CHANGE ONLY"], vm )
  | tooMuchMoney && enoughChange =
    let newVm = (removeProduct p . removeCash change) vm
    in ([Dispense (key p), CollectCurrentAmount, Return change, DisplayTimed "THANK YOU" (Second 5), display newVm], newVm )
  | otherwise =
    let newVm = removeProduct p vm
    in ([Dispense (key p), CollectCurrentAmount, DisplayTimed "THANK YOU" (Second 5), display newVm], newVm )
  where enoughStock = if quantity p > 0 then True else False
        enoughMoney = if total (currentAmount vm) >= price p then True else False
        tooMuchMoney = if total (currentAmount vm) > price p then True else False
        enoughChange = change /= []
        change = makeChange (cash vm) (total(currentAmount vm) - (price p)) []

returnCoins :: State VendingMachine [Instruction]
returnCoins = state $ \vm -> let newVm = vm { currentAmount = [] }
                             in ([ReturnCurrentAmount, display newVm], newVm)

-- private fns

makeChange :: [Coin] -> Float -> [Coin] -> [Coin]
makeChange (x:xs) target acc
  | target <= 0 = []
  | total (x:acc) == target = (x:acc)
  | total (x:acc) > target = []
  | total (x:acc) < target =  makeChange xs target (x:acc)
  | otherwise = makeChange xs target acc
makeChange [] _ _ = []

removeProduct p vm =
  vm {
    stock = replace p (p { quantity =  (quantity p) - 1  }) (stock vm),
    cash = cash vm ++ currentAmount vm,
    currentAmount = []
  }

removeCash :: [Coin] -> VendingMachine -> VendingMachine
removeCash coins vm = vm { cash = filterList coins (cash vm) }
  where filterList [] l = l
        filterList (x:xs) l = filterList xs (filterFirst x l)
        filterFirst _ [] = []
        filterFirst p (x:xs)
            | p == x = xs
            | otherwise = x:filterFirst p xs

replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map (\x -> if (a == x) then b else x)

display vm =
  if a > 0 then (Display . printf "$%.2f" . (/100)) a
  else if cash vm == [] then Display "EXACT CHANGE ONLY"
  else Display "INSERT COIN"
  where a = total $ currentAmount vm

total :: [Coin] -> Float
total coins = (sum . (fmap coinValue)) coins

coinValue :: Coin -> Float
coinValue Penny = 1.0
coinValue Nickel = 5.0
coinValue Dime = 10.0
coinValue Quarter = 25.0

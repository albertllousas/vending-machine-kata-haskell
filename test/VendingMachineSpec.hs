module VendingMachineSpec where

import VendingMachine
import Test.Hspec
import Control.Monad.State

spec :: Spec
spec = do

  describe "Accept Coins" $ do

    let vm = VendingMachine { currentAmount = [], stock = [], cash = [Dime] }

    it "should accept a nickel coin" $ do
      let result = insertCoin Nickel
      runState result vm `shouldBe` ([AcceptCoin, Display "$0.05"], VendingMachine { currentAmount = [Nickel], stock = [], cash = [Dime] })

    it "should accept a dime coin" $ do
      let result = insertCoin Dime
      runState result vm `shouldBe` ([AcceptCoin, Display "$0.10"], VendingMachine { currentAmount = [Dime], stock = [], cash = [Dime] })

    it "should accept a quarter coin" $ do
      let result = insertCoin Quarter
      runState result vm `shouldBe` ([AcceptCoin, Display "$0.25"], VendingMachine { currentAmount = [Quarter], stock = [], cash = [Dime] })

    it "should accept a coin and add it to the current amount" $ do
      let vm = VendingMachine { currentAmount = [Dime, Nickel, Quarter], stock = [], cash = [Dime] }
      let result = insertCoin Nickel
      runState result vm `shouldBe` (
          [AcceptCoin, Display "$0.45"],
          VendingMachine { currentAmount = [Nickel, Dime, Nickel, Quarter], stock = [], cash = [Dime] }
        )

    it "should reject an invalid coin" $ do
      let result = insertCoin Penny
      runState result vm `shouldBe` (
        [RejectCoin, Display "INSERT COIN"],
        VendingMachine { currentAmount = [], cash = [Dime], stock = [] }
        )

  describe "Select Product" $ do

    let cola = Product { key = "cola", price = 100, quantity = 5 }

    let vm = VendingMachine {
      currentAmount = [Quarter, Quarter, Quarter, Quarter],
      stock = [cola],
      cash = []
    }

    it "should select and dispense a product" $ do
      let result = selectProduct "cola"
      runState result vm `shouldBe` (
          [Dispense "cola", CollectCurrentAmount, DisplayTimed "THANK YOU" (Second 5), Display "INSERT COIN"],
          VendingMachine { currentAmount = [], stock = [ cola { quantity = 4 } ], cash = [Quarter, Quarter, Quarter, Quarter] }
        )

    it "should not dispense a product when there is not enough money inserted" $ do
      let vm = VendingMachine { currentAmount = [Quarter], stock = [cola], cash = [] }
      let result = selectProduct "cola"
      runState result vm `shouldBe` (
          [DisplayTimed "PRICE $1.00" (Second 5), Display "$0.25"],
          VendingMachine { currentAmount = [Quarter], stock = [ cola { quantity = 5 } ], cash = [] }
        )

  describe "Make Change" $ do

    let cola = Product { key = "cola", price = 95, quantity = 5 }

    it "should return the remaining amount when a product costs less than the amount inserted" $ do
      let vm = VendingMachine {
            currentAmount = [Quarter, Quarter, Quarter, Quarter],
            stock = [cola],
            cash = [Nickel, Nickel]
          }
      let result = selectProduct "cola"
      runState result vm `shouldBe` (
          [Dispense "cola", CollectCurrentAmount, Return [Nickel], DisplayTimed "THANK YOU" (Second 5), Display "INSERT COIN"],
          VendingMachine { currentAmount = [], stock = [ cola { quantity = 4 } ], cash = [Nickel, Quarter, Quarter, Quarter, Quarter] }
        )

  describe "Return coins" $ do

    it "should return coins when button is pressed" $ do
      let vm = VendingMachine { currentAmount = [Quarter, Quarter], stock = [], cash = [Dime] }
      runState returnCoins vm `shouldBe` (
          [ReturnCurrentAmount, Display "INSERT COIN"],
          VendingMachine { currentAmount = [], stock = [], cash = [Dime] }
        )

  describe "Sold out" $ do

    it "should show that a product is out od stock" $ do
      let cola = Product { key = "cola", price = 95, quantity = 0 }
      let vm = VendingMachine { currentAmount = [], stock = [cola], cash = [Dime] }
      let result = selectProduct "cola"
      runState result vm `shouldBe` ( [DisplayTimed "SOLD OUT" (Second 5), Display "INSERT COIN"], vm )

  describe "Exact Change Only" $ do

    it "should display exact change only when there is no enough cash to make change" $ do
      let cola = Product { key = "cola", price = 95, quantity = 2 }
      let vm = VendingMachine { currentAmount = [Quarter, Quarter, Quarter, Quarter], stock = [cola], cash = [Dime] }
      let result = selectProduct "cola"
      runState result vm `shouldBe` ( [Display "EXACT CHANGE ONLY"], vm )

    it "should display exact change when there is no available cash" $ do
      let vm = VendingMachine { currentAmount = [], stock = [], cash = [] }
      let result = insertCoin Penny
      runState result vm `shouldBe` (
        [RejectCoin, Display "EXACT CHANGE ONLY"],
        VendingMachine { currentAmount = [], cash = [], stock = [] }
        )


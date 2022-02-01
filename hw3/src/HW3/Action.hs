{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia        #-}

module HW3.Action
  where
import Control.Exception (Exception)
import Control.Monad.Reader ( ReaderT(ReaderT) )
import Data.Data (Typeable)
import Data.Set (Set)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving stock (Show, Eq, Typeable, Ord, Enum, Bounded)

data PermissionException =
  PermissionRequired HiPermission
  deriving stock (Show, Eq, Typeable, Ord)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
    deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.
module Test.Wire.API.Golden.Generated.NewUser_user where

import Data.Currency (Alpha (XUA))
import Data.ISO3166_CountryCodes
  ( CountryCode
      ( LY
      ),
  )
import Data.Id (Id (Id, toUUID))
import qualified Data.LanguageCodes
  ( ISO639_1
      ( SN
      ),
  )
import Data.Misc (PlainTextPassword (PlainTextPassword))
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust, fromRight, undefined, (.))
import Wire.API.Team (BindingNewTeam (..), NewTeam (..))
import Wire.API.User
  ( Asset (ImageAsset),
    AssetSize (..),
    BindingNewTeamUser (..),
    ColourId (ColourId, fromColourId),
    Country (Country, fromCountry),
    Email (Email, emailDomain, emailLocal),
    InvitationCode (InvitationCode, fromInvitationCode),
    Language (Language),
    Locale (Locale, lCountry, lLanguage),
    ManagedBy (ManagedByWire),
    Name (Name, fromName),
    NewTeamUser (..),
    NewUser (..),
    NewUserOrigin (..),
    Pict (Pict, fromPict),
    UserIdentity
      ( EmailIdentity,
        PhoneIdentity,
        SSOIdentity
      ),
    emptyNewUser,
  )
import Wire.API.User.Activation (ActivationCode (ActivationCode, fromActivationCode))
import Wire.API.User.Auth (CookieLabel (CookieLabel, cookieLabelText))
import Wire.API.User.Identity (Phone (..), UserSSOId (UserSSOId))

testObject_NewUser_user_1 :: NewUser
testObject_NewUser_user_1 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "V~\14040\38047\NULw\1105603\1077601\&1\73084\1020199%\14699]y*\121297jqM\SYN\74260/\1108497-*\US \RSA\SO}\64347c\60361v [\1022394t\1012213R\181051Y\1036488\&6tg\SYN\1044855+\DLE\99976;\ACKOj\DC3\48593&aD:\nf\1002443!*\DEL"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity = Just (EmailIdentity (Email {emailLocal = "S\ENQX\1076723$\STX\"\1110507e\1015716\24831\1031964L\ETB", emailDomain = "P.b"})),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ ImageAsset "\5112\&5\DC4\1019826\95472`\a\SUBG{?" (Just AssetPreview),
          ImageAsset "something" (Just AssetComplete),
          ImageAsset "KE" Nothing
        ],
      newUserAccentId = Just (ColourId {fromColourId = -7404}),
      newUserEmailCode = Just (ActivationCode {fromActivationCode = fromRight undefined (validate "1YgaHo0=")}),
      newUserPhoneCode = Just (ActivationCode {fromActivationCode = fromRight undefined (validate "z1OeJQ==")}),
      newUserOrigin =
        Just
          ( NewUserOriginInvitationCode
              ( InvitationCode
                  { fromInvitationCode = fromRight undefined (validate "DhBvokHtVbWSKbWi0_IATMGH3P8DLEOw5YIcYg==")
                  }
              )
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = "\186640\&15XwT\991660: \DC1Z+\ty\94985\SOH"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.SN, lCountry = Just (Country {fromCountry = LY})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "\143961%5T5'<IQtf]\13622\EOT\1019095\FS+\f\98493\FS$\fY\RSG=p\182047V\DC2E_ni\\Q:\118823>\17286\7398\ENQ-\1063783\100891\&7W@\59062)!$%v{\f\n_I6\1088622\52764]r\1105300\61079\STXGi_L\ENQ@tr<\35715\&2Dr\16519\\\v8\49277\DC4\1069631e\b\190386\71324srN\34600\26071Qk+\36197\999209O\\c6\1032813X\1026685\1074390VV\\\999471^\1105556\DC4(P~y\SI(\nrO\1037710U=$\1038971k\1011736\&7.\NAK[dn\1061566\31927_\NUL\997265\vNVd\54706z\1029333pV6\RS\166743#/m\1065646w\NAK\27792u\144303\SIs\DC1\136497^A\95500>\SUB#\EMsC!3#\59953`\159877q\65860\\VrnT\DLE\SYN\1060441\DC4\STX\156538\1003845\DC2d \1028483#\CAN\179878/k\14627X\"I\SIO,`GU+\DC1\DEL\"\n\47090n)\ESC\1059861x\1018430\1097583%\DC2\SIVr\f\1044385H`\128647W\FS\NAK\1050334vii\FS\a\ENQ\1005180&d\GS\146823\991562.\1090052j\1008159$=a_s\DLEQ\1020394\SO\f\ETX\1019724B\ENQ\CANL\STX_ZX\NAK h_sGj)\1047298|\NUL\SI\rlUN)\ACK\DC1`8\f\1018610\999181\b,A\DC1\tt/0lT\1071777\a}\SYNj\SI\az|\ENQ\152944J,26\1022981\ETX9\11179\&0\EMw'\NULO&g\USF0\1001389kg\STX\DC1|Q\1048680\SUBM\131896\1038590vuPgVp\180615)/<W\1077985R\1069905,\39037\v\145751y\DLE\vS\SUB(\DC4!\SI\181381\RS\988082U*(\194576\&4rX\SO\8572A\1035058\SO\DC4z9\1050749\1083893{^\1053567\168917\n\b\1105884rYL\31991h\167262?J\ETBn45X.L\1098255^{ot|\EMq\34593\a(\137115I\v\ESC\168793\132359\ENQ.\17815y0a\32961I\DEL\NAKKk\154911\NAK\34647\1063811\SIM`\178041\47494N\NAK9`\1026852\FS@Y~\SOL\1018994\1098118o3\95471;\1011338\1010407\DC3fz\SUB\1025379(}.)[!'2LJS\rBK\3667\ETX5<u`\SO\"\12511i\1018516H&\DLEP\1106835#\188694t1\ENQ{\ENQ\1083098\fS\NUL=\SYN\1080920\ESC\30501\ESC$\1044616\&3\1687@GKvG/\1000255\157153,\176780i\1028057\&9TW\ESCoe\f\FSf\1110205S\GS\42634\998348g\1053453{\DC2\63337fg\DLE\1095355\DLEbc\1030987\1023379f_}.@\17549{r~{\f[?`\1050469\1070751[!\DC2B7%"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByWire
    }

testObject_NewUser_user_2 :: NewUser
testObject_NewUser_user_2 = emptyNewUser (Name {fromName = "\NUL`)\a|>}\EM5z\70179\t>w\SO\1007537"})

testObject_NewUser_user_3 :: NewUser
testObject_NewUser_user_3 = testObject_NewUser_user_2 {newUserExpiresIn = Just (unsafeRange 378975)}

inv_code :: InvitationCode
inv_code =
  InvitationCode
    { fromInvitationCode =
        fromRight
          undefined
          (validate "RUne0vse27qsm5jxGmL0xQaeuEOqcqr65rU=")
    }

testObject_NewUser_user_4 :: NewUser
testObject_NewUser_user_4 =
  ( emptyNewUser
      (Name {fromName = "test name"})
  )
    { newUserOrigin = Just (NewUserOriginInvitationCode inv_code)
    }

testObject_NewUser_user_5 :: NewUser
testObject_NewUser_user_5 =
  ( emptyNewUser
      (Name {fromName = "test name"})
  )
    { newUserOrigin = Just (NewUserOriginTeamUser (NewTeamMember inv_code)),
      newUserPassword = Just (PlainTextPassword "123456")
    }

testObject_NewUser_user_6 :: NewUser
testObject_NewUser_user_6 =
  ( emptyNewUser
      (Name {fromName = "test name"})
  )
    { newUserOrigin = Just (NewUserOriginTeamUser (NewTeamMemberSSO tid)),
      newUserIdentity = Just (SSOIdentity (UserSSOId "some" "thing") Nothing Nothing)
    }
  where
    tid = Id (fromJust (UUID.fromString "00007b0e-0000-3489-0000-075c00005be7"))

testObject_NewUser_user_7 :: NewUser
testObject_NewUser_user_7 =
  ( emptyNewUser
      (Name {fromName = "test name"})
  )
    { newUserOrigin = Just (NewUserOriginTeamUser (NewTeamCreator user)),
      newUserIdentity = Just (PhoneIdentity (Phone "+12345678")),
      newUserPassword = Just (PlainTextPassword "123456")
    }
  where
    user =
      BindingNewTeamUser
        { bnuTeam =
            BindingNewTeam
              ( NewTeam
                  { _newTeamName =
                      unsafeRange
                        "\fe\ENQ\1011760zm",
                    _newTeamIcon =
                      unsafeRange
                        "Coq\52427\v\182208",
                    _newTeamIconKey =
                      Just
                        ( unsafeRange
                            "\ACKc\151665L ,"
                        ),
                    _newTeamMembers = Nothing
                  }
              ),
          bnuCurrency = Just XUA
        }

testObject_NewUser_user_8 :: NewUser
testObject_NewUser_user_8 =
  ( emptyNewUser
      (Name {fromName = "test name"})
  )
    { newUserOrigin = Just (NewUserOriginTeamUser (NewTeamMember inv_code)),
      newUserIdentity = Just (PhoneIdentity (Phone "+12345678")),
      newUserPassword = Just (PlainTextPassword "123456")
    }

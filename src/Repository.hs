module Repository where
import TypeOperators ( Contain )
import Spies (SpecifiedSpy)

find :: Contain a b => (SpecifiedSpy a -> Maybe(SpecifiedSpy b)) -> [SpecifiedSpy a] -> [Maybe(SpecifiedSpy b)]
find specification = foldr (\ spy -> (<>) [specification spy]) []
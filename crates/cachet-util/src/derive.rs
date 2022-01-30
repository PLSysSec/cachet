#[macro_export]
macro_rules! box_from {
    (
        $src:ty => $dst:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::convert::From<$src> for $dst
        $($(where $($where_clause)+)?)?
        {
            fn from(src: $src) -> Self {
                ::std::convert::From::from(Box::new(src))
            }
        }
    };
}

#[macro_export]
macro_rules! chain_from {
    (
        $src:ty => $mid:ty => $dst:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::convert::From<$src> for $dst
        $($(where $($where_clause)+)?)?
        {
            fn from(src: $src) -> Self {
                ::std::convert::From::from(::std::convert::Into::<$mid>::into(src))
            }
        }
    };
}

#[macro_export]
macro_rules! deref_from {
    (
        $src:ty => $dst:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::convert::From<$src> for $dst
        $($(where $($where_clause)+)?)?
        {
            fn from(src: $src) -> Self {
                ::std::convert::From::from(*src)
            }
        }
    };
}

#[macro_export]
macro_rules! deref_index {
    (
        $parent:ty[$index:ty] => $output:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::ops::Index<$index> for $parent
        $($(where $($where_clause)+)?)?
        {
            type Output = $output;

            fn index(&self, index: $index) -> &Self::Output {
                &self[*index]
            }
        }

        impl$($(<$($generic_params),+>)?)? ::std::ops::IndexMut<$index> for $parent
        $($(where $($where_clause)+)?)?
        {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                &mut self[*index]
            }
        }
    };
}

#[macro_export]
macro_rules! field_index {
    (
        $parent:ty:$field:ident[$index:ty] => $output:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::ops::Index<$index> for $parent
        $($(where $($where_clause)+)?)?
        {
            type Output = $output;

            fn index(&self, index: $index) -> &Self::Output {
                &self.$field[index]
            }
        }

        impl$($(<$($generic_params),+>)?)? ::std::ops::IndexMut<$index> for $parent
        $($(where $($where_clause)+)?)?
        {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                &mut self.$field[index]
            }
        }

        $crate::deref_index!(
            $parent[&$index] => $output
            $(| $(<$($generic_params),+>)? $(where $($where_clause)+)?)?
        );
    };
}

#[macro_export]
macro_rules! typed_index {
    ($vis:vis $index:ident) => {
        #[derive(
            ::derive_more::Display,
            ::derive_more::From,
            ::derive_more::Into,
            ::std::clone::Clone,
            ::std::cmp::Eq,
            ::std::cmp::Ord,
            ::std::cmp::PartialEq,
            ::std::cmp::PartialOrd,
            ::std::hash::Hash,
            ::std::marker::Copy,
        )]
        $vis struct $index(usize);

        // Use a compact representation for `Debug`-formatting indexes, even in
        // pretty mode.
        impl ::std::fmt::Debug for $index {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
                write!(f, "{}({})", stringify!($index), self.0)
            }
        }
    };
}

#[macro_export]
macro_rules! typed_field_index {
    (
        $parent:ty:$field:ident[$vis:vis $index:ident] => $output:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        $crate::typed_index!($vis $index);
        $crate::field_index!(
            $parent:$field[$index] => $output
            $(| $(<$($generic_params),+>)? $(where $($where_clause)+)?)?
        );
    };
}

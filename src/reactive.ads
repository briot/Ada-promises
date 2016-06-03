--  Various packages to support reactive programming.
--  See http://reactivex.io
--
--  These packages provide a better approach to the classical
--  Observer design pattern.
--  They also encompass the notion of promises that is used to
--  synchronize between asynchronous tasks.

package Reactive is

   --------------
   -- IFreeable --
   --------------

   type IFreeable is interface;
   type Freeable_Access is access all IFreeable'Class;
   --  a general interface for objects that have an explicit Free
   --  primitive operation.

   procedure Free (Self : in out IFreeable) is null;
   --  Free internal data of Self

   procedure Free (Self : in out Freeable_Access);
   --  Free self, via its primitive operation, and then free the pointer

end Reactive;

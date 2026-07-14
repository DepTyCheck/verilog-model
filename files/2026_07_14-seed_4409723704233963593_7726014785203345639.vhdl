-- Seed: 4409723704233963593,7726014785203345639

use std.reflection.all;

entity dakqdng is
  port (l : inout physical_value_mirror; rjr : inout floating_subtype_mirror);
end dakqdng;

architecture iymmgxnp of dakqdng is
  
begin
  
end iymmgxnp;

entity nobtysnlqw is
  port (mylpzaze : in time; v : inout real);
end nobtysnlqw;

use std.reflection.all;

architecture nhgad of nobtysnlqw is
  shared variable z : floating_subtype_mirror;
  shared variable pxffaoxdq : physical_value_mirror;
begin
  ubnxrsjj : entity work.dakqdng
    port map (l => pxffaoxdq, rjr => z);
  
  -- Single-driven assignments
  v <= 2#1000.0_0_0_0_1#;
end nhgad;

use std.reflection.all;

entity m is
  port (gsvbg : in time; l : inout physical_value_mirror);
end m;

use std.reflection.all;

architecture vqfvk of m is
  shared variable wauxhwx : floating_subtype_mirror;
begin
  w : entity work.dakqdng
    port map (l => l, rjr => wauxhwx);
end vqfvk;



-- Seed after: 5050917307151501787,7726014785203345639

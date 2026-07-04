-- Seed: 17365683272078853786,6290177331721581829

use std.reflection.all;

entity uu is
  port (muhvpzv : out time; cuv : inout record_value_mirror);
end uu;

architecture y of uu is
  
begin
  -- Single-driven assignments
  muhvpzv <= 30 ms;
end y;

use std.reflection.all;

entity xh is
  port (zudhdzfu : inout record_value_mirror);
end xh;

use std.reflection.all;

architecture qq of xh is
  shared variable w : record_value_mirror;
  signal aamd : time;
  signal jllakuaknt : time;
  shared variable ojbx : record_value_mirror;
  signal cdbqj : time;
begin
  jaryo : entity work.uu
    port map (muhvpzv => cdbqj, cuv => ojbx);
  jdsk : entity work.uu
    port map (muhvpzv => jllakuaknt, cuv => zudhdzfu);
  npaok : entity work.uu
    port map (muhvpzv => aamd, cuv => w);
end qq;



-- Seed after: 5569500132875455573,6290177331721581829

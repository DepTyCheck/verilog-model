-- Seed: 10312317762121061166,6290177331721581829

use std.reflection.all;

entity pesocd is
  port (elfw : inout real; ohajtk : inout integer_subtype_mirror; qouqk : inout record_subtype_mirror; wmmdppkirt : inout file_value_mirror);
end pesocd;

architecture scoemxj of pesocd is
  
begin
  -- Single-driven assignments
  elfw <= elfw;
end scoemxj;

library ieee;
use ieee.std_logic_1164.all;

entity dixyovh is
  port (qhcki : buffer std_logic);
end dixyovh;

use std.reflection.all;

architecture abwntow of dixyovh is
  shared variable ughou : file_value_mirror;
  shared variable intg : record_subtype_mirror;
  shared variable udaioe : integer_subtype_mirror;
  signal kvunr : real;
  shared variable qzgqhn : file_value_mirror;
  shared variable z : record_subtype_mirror;
  shared variable rcjxu : integer_subtype_mirror;
  signal tjdrwkjxxj : real;
  shared variable ulid : file_value_mirror;
  shared variable gicmsqxryb : record_subtype_mirror;
  shared variable isw : integer_subtype_mirror;
  signal xuwqu : real;
begin
  ynoooanzpf : entity work.pesocd
    port map (elfw => xuwqu, ohajtk => isw, qouqk => gicmsqxryb, wmmdppkirt => ulid);
  bovmtvqpqp : entity work.pesocd
    port map (elfw => tjdrwkjxxj, ohajtk => rcjxu, qouqk => z, wmmdppkirt => qzgqhn);
  wx : entity work.pesocd
    port map (elfw => kvunr, ohajtk => udaioe, qouqk => intg, wmmdppkirt => ughou);
  
  -- Multi-driven assignments
  qhcki <= qhcki;
  qhcki <= qhcki;
end abwntow;

use std.reflection.all;

entity nnqfkyhp is
  port (uqadiyr : inout record_subtype_mirror; kaaxc : linkage integer; hcxa : out integer);
end nnqfkyhp;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture hufwmiysbq of nnqfkyhp is
  shared variable qbezvqzs : file_value_mirror;
  shared variable em : record_subtype_mirror;
  shared variable besvcdji : integer_subtype_mirror;
  signal dnjaknsxw : real;
  shared variable duqdys : file_value_mirror;
  shared variable gjz : record_subtype_mirror;
  shared variable rkrvyo : integer_subtype_mirror;
  signal upvmkau : real;
  signal vtmxl : std_logic;
begin
  pihjjn : entity work.dixyovh
    port map (qhcki => vtmxl);
  xwzsadzg : entity work.pesocd
    port map (elfw => upvmkau, ohajtk => rkrvyo, qouqk => gjz, wmmdppkirt => duqdys);
  b : entity work.pesocd
    port map (elfw => dnjaknsxw, ohajtk => besvcdji, qouqk => em, wmmdppkirt => qbezvqzs);
  
  -- Single-driven assignments
  hcxa <= 16#1#;
  
  -- Multi-driven assignments
  vtmxl <= '-';
  vtmxl <= 'H';
end hufwmiysbq;



-- Seed after: 4370234847583605463,6290177331721581829

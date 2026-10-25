-- Seed: 13437490024375370949,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity sd is
  port (bl : inout integer; v : inout integer; ktrrjqat : buffer std_logic);
end sd;

architecture i of sd is
  
begin
  -- Single-driven assignments
  v <= v;
  bl <= 16#E_B_2_4#;
end i;

entity tpiuzcme is
  port (ayyn : inout integer; mccbkwu : linkage real);
end tpiuzcme;

library ieee;
use ieee.std_logic_1164.all;

architecture qqxn of tpiuzcme is
  signal pdodhagftf : integer;
  signal exsdmsmmp : integer;
  signal tyaqlvyks : integer;
  signal dprl : std_logic;
  signal cltufmzjio : integer;
  signal lixxlhrgvg : integer;
  signal pzhlg : std_logic;
  signal im : integer;
  signal mfpycsw : integer;
begin
  uzudn : entity work.sd
    port map (bl => mfpycsw, v => im, ktrrjqat => pzhlg);
  mgjvmqb : entity work.sd
    port map (bl => lixxlhrgvg, v => cltufmzjio, ktrrjqat => dprl);
  rpbpieu : entity work.sd
    port map (bl => tyaqlvyks, v => exsdmsmmp, ktrrjqat => dprl);
  qf : entity work.sd
    port map (bl => ayyn, v => pdodhagftf, ktrrjqat => pzhlg);
  
  -- Multi-driven assignments
  dprl <= pzhlg;
  pzhlg <= pzhlg;
  pzhlg <= 'X';
  pzhlg <= '0';
end qqxn;



-- Seed after: 1800863675995942401,13788294582928374589

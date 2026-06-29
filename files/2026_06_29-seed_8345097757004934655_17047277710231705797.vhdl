-- Seed: 8345097757004934655,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity fipwgclin is
  port (qnlemfmszm : in real; zno : buffer integer; kdykk : inout std_logic_vector(4 to 4));
end fipwgclin;

architecture ona of fipwgclin is
  
begin
  -- Single-driven assignments
  zno <= 0_3;
end ona;

entity nbp is
  port (qawxbkxfgy : out integer);
end nbp;

library ieee;
use ieee.std_logic_1164.all;

architecture dsdqpgbqsj of nbp is
  signal a : integer;
  signal z : real;
  signal ftcxasv : real;
  signal qsknggz : std_logic_vector(4 to 4);
  signal wtf : integer;
  signal kczqlxzg : real;
  signal j : std_logic_vector(4 to 4);
  signal mvxebmciu : integer;
  signal puvoppi : real;
begin
  nb : entity work.fipwgclin
    port map (qnlemfmszm => puvoppi, zno => mvxebmciu, kdykk => j);
  ai : entity work.fipwgclin
    port map (qnlemfmszm => kczqlxzg, zno => wtf, kdykk => qsknggz);
  t : entity work.fipwgclin
    port map (qnlemfmszm => ftcxasv, zno => qawxbkxfgy, kdykk => qsknggz);
  pcndazuhge : entity work.fipwgclin
    port map (qnlemfmszm => z, zno => a, kdykk => j);
  
  -- Multi-driven assignments
  qsknggz <= "U";
  j <= (others => 'Z');
  j <= (others => 'W');
  qsknggz <= "X";
end dsdqpgbqsj;

library ieee;
use ieee.std_logic_1164.all;

entity ogtiizpxtn is
  port (ht : in boolean_vector(2 downto 1); shneyoc : out std_logic; ksmiziuse : linkage character; ioaaxtpw : inout std_logic_vector(4 to 4));
end ogtiizpxtn;

library ieee;
use ieee.std_logic_1164.all;

architecture okhgfrfsha of ogtiizpxtn is
  signal cqgv : std_logic_vector(4 to 4);
  signal tdvgzpb : integer;
  signal opgazq : real;
  signal xlntw : integer;
  signal bsxxmyodoq : real;
begin
  jxm : entity work.fipwgclin
    port map (qnlemfmszm => bsxxmyodoq, zno => xlntw, kdykk => ioaaxtpw);
  jdqvo : entity work.fipwgclin
    port map (qnlemfmszm => opgazq, zno => tdvgzpb, kdykk => cqgv);
  
  -- Single-driven assignments
  bsxxmyodoq <= 16#9_5_0_C_5.6#;
  opgazq <= 2#0010.10#;
end okhgfrfsha;



-- Seed after: 6663234233870931839,17047277710231705797

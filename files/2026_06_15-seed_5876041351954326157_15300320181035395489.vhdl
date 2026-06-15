-- Seed: 5876041351954326157,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity sxoco is
  port (ivgpjo : inout std_logic; xd : out time; eixlesua : out real; xkm : linkage string(4 downto 3));
end sxoco;

architecture rq of sxoco is
  
begin
  -- Single-driven assignments
  eixlesua <= 21.2;
  xd <= 16#3# ms;
  
  -- Multi-driven assignments
  ivgpjo <= 'H';
  ivgpjo <= '0';
end rq;

library ieee;
use ieee.std_logic_1164.all;

entity ycva is
  port (mpzoqwn : out time; rhxkq : out std_logic_vector(1 to 2); yzd : inout real; x : inout std_logic_vector(3 downto 0));
end ycva;

library ieee;
use ieee.std_logic_1164.all;

architecture ezmm of ycva is
  signal cwl : string(4 downto 3);
  signal dvhifhk : time;
  signal zjnrx : string(4 downto 3);
  signal gcofq : real;
  signal yj : std_logic;
  signal bxs : string(4 downto 3);
  signal ma : real;
  signal eov : time;
  signal smpjdwdru : string(4 downto 3);
  signal jdicdyydvb : real;
  signal dwhzz : time;
  signal yt : std_logic;
begin
  puv : entity work.sxoco
    port map (ivgpjo => yt, xd => dwhzz, eixlesua => jdicdyydvb, xkm => smpjdwdru);
  wophbi : entity work.sxoco
    port map (ivgpjo => yt, xd => eov, eixlesua => ma, xkm => bxs);
  woecujc : entity work.sxoco
    port map (ivgpjo => yj, xd => mpzoqwn, eixlesua => gcofq, xkm => zjnrx);
  vh : entity work.sxoco
    port map (ivgpjo => yt, xd => dvhifhk, eixlesua => yzd, xkm => cwl);
  
  -- Multi-driven assignments
  x <= ('0', '-', 'U', 'L');
  yj <= '1';
  yt <= 'L';
end ezmm;

library ieee;
use ieee.std_logic_1164.all;

entity rsf is
  port (ehfb : inout integer_vector(2 to 4); qace : linkage std_logic_vector(4 downto 4); ksjn : out integer);
end rsf;

architecture fjgycq of rsf is
  
begin
  -- Single-driven assignments
  ksjn <= 16#8_8_D#;
  ehfb <= (16#6_5#, 2#1#, 4422);
end fjgycq;



-- Seed after: 16910190912785630860,15300320181035395489

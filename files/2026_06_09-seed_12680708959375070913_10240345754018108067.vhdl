-- Seed: 12680708959375070913,10240345754018108067



entity s is
  port (ktdixa : out integer);
end s;



architecture mtdyjj of s is
  
begin
  
end mtdyjj;

library ieee;
use ieee.std_logic_1164.all;

entity hu is
  port (xyuj : inout bit_vector(0 downto 2); mwi : in integer; q : buffer std_logic);
end hu;



architecture efbsxwrpr of hu is
  signal jjwqgeh : integer;
  signal ba : integer;
  signal xqgq : integer;
  signal eqll : integer;
begin
  eskjyhkm : entity work.s
    port map (ktdixa => eqll);
  yazzwid : entity work.s
    port map (ktdixa => xqgq);
  rsak : entity work.s
    port map (ktdixa => ba);
  xunzsg : entity work.s
    port map (ktdixa => jjwqgeh);
end efbsxwrpr;



entity p is
  port (ielovlkxbj : inout time; lhklattem : inout integer);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture m of p is
  signal r : integer;
  signal iysgwasdm : integer;
  signal rpgc : std_logic;
  signal l : integer;
  signal lwxxbnwkn : bit_vector(0 downto 2);
begin
  jmburertu : entity work.hu
    port map (xyuj => lwxxbnwkn, mwi => l, q => rpgc);
  tjgtf : entity work.s
    port map (ktdixa => iysgwasdm);
  qbjlup : entity work.s
    port map (ktdixa => r);
  jxpblkqh : entity work.s
    port map (ktdixa => l);
end m;



-- Seed after: 14440627297059449131,10240345754018108067

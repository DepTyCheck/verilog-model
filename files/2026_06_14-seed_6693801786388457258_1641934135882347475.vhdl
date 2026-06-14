-- Seed: 6693801786388457258,1641934135882347475

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (bvotjnsk : out std_logic_vector(2 downto 4); coojugxtcs : out bit_vector(3 downto 3));
end s;



architecture x of s is
  
begin
  
end x;



entity cdbqhovjp is
  port (qsb : out integer_vector(0 to 4));
end cdbqhovjp;

library ieee;
use ieee.std_logic_1164.all;

architecture ahvwhl of cdbqhovjp is
  signal ixxo : bit_vector(3 downto 3);
  signal tebvbuxu : std_logic_vector(2 downto 4);
  signal hnuofxjvjt : bit_vector(3 downto 3);
  signal xptderzpdi : bit_vector(3 downto 3);
  signal ynuh : std_logic_vector(2 downto 4);
begin
  jvwrtl : entity work.s
    port map (bvotjnsk => ynuh, coojugxtcs => xptderzpdi);
  sibh : entity work.s
    port map (bvotjnsk => ynuh, coojugxtcs => hnuofxjvjt);
  sriqkiyvrl : entity work.s
    port map (bvotjnsk => tebvbuxu, coojugxtcs => ixxo);
end ahvwhl;

library ieee;
use ieee.std_logic_1164.all;

entity lv is
  port (fdbhxdarcm : inout real; bjfxc : inout integer; vkeggmkt : out std_logic_vector(0 downto 4));
end lv;



architecture dsutwn of lv is
  
begin
  
end dsutwn;



entity bwgwupidrc is
  port (gymslfef : buffer integer; kx : in integer; dqr : buffer character; f : linkage time);
end bwgwupidrc;

library ieee;
use ieee.std_logic_1164.all;

architecture opkqenqnsh of bwgwupidrc is
  signal bikelwxr : std_logic_vector(0 downto 4);
  signal moljoykq : real;
  signal svczwoq : bit_vector(3 downto 3);
  signal detfllgj : std_logic_vector(2 downto 4);
  signal ovzl : bit_vector(3 downto 3);
  signal aywh : std_logic_vector(2 downto 4);
  signal fkfs : integer;
  signal tfbubh : real;
begin
  h : entity work.lv
    port map (fdbhxdarcm => tfbubh, bjfxc => fkfs, vkeggmkt => aywh);
  pxtvzs : entity work.s
    port map (bvotjnsk => aywh, coojugxtcs => ovzl);
  znlxizmiof : entity work.s
    port map (bvotjnsk => detfllgj, coojugxtcs => svczwoq);
  fq : entity work.lv
    port map (fdbhxdarcm => moljoykq, bjfxc => gymslfef, vkeggmkt => bikelwxr);
end opkqenqnsh;



-- Seed after: 5119999685284129796,1641934135882347475

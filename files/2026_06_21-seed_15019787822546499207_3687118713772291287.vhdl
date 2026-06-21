-- Seed: 15019787822546499207,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity yjnun is
  port (y : buffer std_logic; ie : buffer bit_vector(4 downto 1); twwglwzhlg : inout std_logic; knbftj : out std_logic_vector(4 downto 1));
end yjnun;

architecture qoytpajk of yjnun is
  
begin
  -- Single-driven assignments
  ie <= ('0', '0', '0', '0');
  
  -- Multi-driven assignments
  twwglwzhlg <= 'U';
  twwglwzhlg <= '0';
  y <= 'L';
  knbftj <= ('U', 'W', 'L', '0');
end qoytpajk;

library ieee;
use ieee.std_logic_1164.all;

entity hkmztxy is
  port (wnkovawqsu : linkage time; krjhllxg : in std_logic_vector(0 to 2));
end hkmztxy;

library ieee;
use ieee.std_logic_1164.all;

architecture xrwk of hkmztxy is
  signal zdurkqk : std_logic;
  signal wjumjhk : bit_vector(4 downto 1);
  signal uzgxsieiuv : std_logic_vector(4 downto 1);
  signal b : bit_vector(4 downto 1);
  signal dkieyxzl : std_logic;
  signal quvccnb : std_logic_vector(4 downto 1);
  signal z : bit_vector(4 downto 1);
  signal cpqxdjm : std_logic;
begin
  nysrzzf : entity work.yjnun
    port map (y => cpqxdjm, ie => z, twwglwzhlg => cpqxdjm, knbftj => quvccnb);
  kkavo : entity work.yjnun
    port map (y => dkieyxzl, ie => b, twwglwzhlg => dkieyxzl, knbftj => uzgxsieiuv);
  kdsxh : entity work.yjnun
    port map (y => dkieyxzl, ie => wjumjhk, twwglwzhlg => zdurkqk, knbftj => uzgxsieiuv);
  
  -- Multi-driven assignments
  dkieyxzl <= 'L';
  zdurkqk <= 'X';
  zdurkqk <= '0';
end xrwk;

library ieee;
use ieee.std_logic_1164.all;

entity azf is
  port (cpb : inout std_logic_vector(4 downto 3));
end azf;

library ieee;
use ieee.std_logic_1164.all;

architecture swueuj of azf is
  signal dwxhrghy : std_logic;
  signal tjvfwyjuq : bit_vector(4 downto 1);
  signal pyynrbokc : std_logic_vector(4 downto 1);
  signal wuf : bit_vector(4 downto 1);
  signal fj : std_logic;
  signal bfzt : std_logic_vector(0 to 2);
  signal fdajiyirei : time;
begin
  vroh : entity work.hkmztxy
    port map (wnkovawqsu => fdajiyirei, krjhllxg => bfzt);
  ydhuetdyl : entity work.yjnun
    port map (y => fj, ie => wuf, twwglwzhlg => fj, knbftj => pyynrbokc);
  sfskqvcbgo : entity work.yjnun
    port map (y => fj, ie => tjvfwyjuq, twwglwzhlg => dwxhrghy, knbftj => pyynrbokc);
  
  -- Multi-driven assignments
  pyynrbokc <= "U1Z-";
  cpb <= ('U', 'L');
  cpb <= ('X', 'H');
end swueuj;

library ieee;
use ieee.std_logic_1164.all;

entity vtuufohjqu is
  port (cpvhdkf : linkage std_logic; axajh : buffer boolean; qrru : inout std_logic);
end vtuufohjqu;

architecture cqhhd of vtuufohjqu is
  
begin
  
end cqhhd;



-- Seed after: 15302359498709000974,3687118713772291287

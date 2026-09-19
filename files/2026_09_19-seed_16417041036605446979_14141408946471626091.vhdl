-- Seed: 16417041036605446979,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity apktwhtpc is
  port (ukjsynxa : buffer integer; sjii : in std_logic_vector(3 downto 2));
end apktwhtpc;

architecture nnlsmaz of apktwhtpc is
  
begin
  -- Single-driven assignments
  ukjsynxa <= 8#1_5#;
end nnlsmaz;

library ieee;
use ieee.std_logic_1164.all;

entity mdxz is
  port (zlxgrk : in std_logic_vector(3 downto 2); xbqvvm : linkage std_logic; quyb : out std_logic);
end mdxz;

architecture gpg of mdxz is
  signal ei : integer;
begin
  bp : entity work.apktwhtpc
    port map (ukjsynxa => ei, sjii => zlxgrk);
  
  -- Multi-driven assignments
  quyb <= 'U';
  quyb <= quyb;
  quyb <= 'X';
end gpg;

entity tyrdaabwe is
  port (kidsblqvyb : inout integer; xulvhchob : in integer; s : buffer string(3 to 3));
end tyrdaabwe;

library ieee;
use ieee.std_logic_1164.all;

architecture meawktbh of tyrdaabwe is
  signal xkybm : std_logic;
  signal pr : std_logic_vector(3 downto 2);
  signal pvdg : std_logic_vector(3 downto 2);
  signal ilefnucvq : integer;
begin
  pq : entity work.apktwhtpc
    port map (ukjsynxa => ilefnucvq, sjii => pvdg);
  p : entity work.mdxz
    port map (zlxgrk => pr, xbqvvm => xkybm, quyb => xkybm);
  
  -- Single-driven assignments
  kidsblqvyb <= 111;
  s <= s;
  
  -- Multi-driven assignments
  xkybm <= xkybm;
  xkybm <= xkybm;
end meawktbh;



-- Seed after: 2846730305323025791,14141408946471626091

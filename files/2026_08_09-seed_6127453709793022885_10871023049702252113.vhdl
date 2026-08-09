-- Seed: 6127453709793022885,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity bdxbohqf is
  port (pcwffgo : in std_logic_vector(3 downto 0); qocx : in bit);
end bdxbohqf;

architecture x of bdxbohqf is
  
begin
  
end x;

library ieee;
use ieee.std_logic_1164.all;

entity owbvucf is
  port (d : inout integer; pstjufo : out std_logic);
end owbvucf;

library ieee;
use ieee.std_logic_1164.all;

architecture xpnyr of owbvucf is
  signal stbut : std_logic_vector(3 downto 0);
  signal mmpjvyr : bit;
  signal o : std_logic_vector(3 downto 0);
  signal mhelplynig : bit;
  signal reulprzf : std_logic_vector(3 downto 0);
begin
  tqpkbtng : entity work.bdxbohqf
    port map (pcwffgo => reulprzf, qocx => mhelplynig);
  meqqboqwvu : entity work.bdxbohqf
    port map (pcwffgo => o, qocx => mmpjvyr);
  wftynl : entity work.bdxbohqf
    port map (pcwffgo => stbut, qocx => mhelplynig);
  
  -- Single-driven assignments
  mhelplynig <= mhelplynig;
end xpnyr;

entity gmyqv is
  port (q : out real);
end gmyqv;

library ieee;
use ieee.std_logic_1164.all;

architecture cyzm of gmyqv is
  signal uudnuko : std_logic;
  signal qxwpkahn : integer;
begin
  vsrhayq : entity work.owbvucf
    port map (d => qxwpkahn, pstjufo => uudnuko);
  
  -- Single-driven assignments
  q <= q;
  
  -- Multi-driven assignments
  uudnuko <= 'U';
end cyzm;



-- Seed after: 18424992802263776193,10871023049702252113

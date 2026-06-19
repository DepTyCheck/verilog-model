-- Seed: 17299695738067184516,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity dzvd is
  port (jpr : buffer time; ya : out std_logic_vector(2 downto 2); xhecmvdw : inout boolean; uixoph : in std_logic);
end dzvd;

architecture sdfusfom of dzvd is
  
begin
  -- Single-driven assignments
  xhecmvdw <= FALSE;
  jpr <= 22132.1413 us;
  
  -- Multi-driven assignments
  ya <= "U";
end sdfusfom;

entity snab is
  port (quatf : buffer time; mzulseiw : out bit_vector(2 downto 3); vazcs : buffer real);
end snab;

library ieee;
use ieee.std_logic_1164.all;

architecture nuxdhbmozz of snab is
  signal lltdbk : std_logic;
  signal uhcmj : boolean;
  signal yvi : std_logic_vector(2 downto 2);
begin
  se : entity work.dzvd
    port map (jpr => quatf, ya => yvi, xhecmvdw => uhcmj, uixoph => lltdbk);
  
  -- Single-driven assignments
  vazcs <= 2#00.01111#;
  mzulseiw <= (others => '0');
  
  -- Multi-driven assignments
  yvi <= (others => 'Z');
  lltdbk <= '1';
  yvi <= (others => 'X');
  yvi <= "1";
end nuxdhbmozz;



-- Seed after: 17628799948725126254,3108530264173481209

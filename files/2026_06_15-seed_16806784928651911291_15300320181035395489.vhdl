-- Seed: 16806784928651911291,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity ch is
  port (ijcqcxo : linkage std_logic);
end ch;

architecture gjcdqtt of ch is
  
begin
  
end gjcdqtt;

entity jrmcwru is
  port (zcdhi : out bit_vector(3 downto 4); clysrfjla : linkage boolean_vector(2 downto 0));
end jrmcwru;

library ieee;
use ieee.std_logic_1164.all;

architecture b of jrmcwru is
  signal eqxxfuyur : std_logic;
  signal sytq : std_logic;
  signal blii : std_logic;
begin
  htmzcbxnfq : entity work.ch
    port map (ijcqcxo => blii);
  owzqaqcrz : entity work.ch
    port map (ijcqcxo => sytq);
  ontn : entity work.ch
    port map (ijcqcxo => eqxxfuyur);
  niz : entity work.ch
    port map (ijcqcxo => eqxxfuyur);
  
  -- Single-driven assignments
  zcdhi <= (others => '0');
  
  -- Multi-driven assignments
  blii <= '0';
  blii <= 'L';
  sytq <= 'W';
  blii <= 'U';
end b;



-- Seed after: 14063886281101639909,15300320181035395489

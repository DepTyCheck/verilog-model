-- Seed: 12131953989757262889,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity uqpijd is
  port (wfv : linkage std_logic_vector(3 downto 2); z : out bit; xal : in integer);
end uqpijd;

architecture ddi of uqpijd is
  
begin
  
end ddi;

library ieee;
use ieee.std_logic_1164.all;

entity tpmbtsba is
  port (xavxywh : in real; pywkzde : buffer real_vector(1 to 0); jweppumik : inout std_logic_vector(3 downto 2));
end tpmbtsba;

library ieee;
use ieee.std_logic_1164.all;

architecture pjvodfcepa of tpmbtsba is
  signal gcjdwn : bit;
  signal kempjlvx : std_logic_vector(3 downto 2);
  signal rpmiofqj : integer;
  signal nsx : bit;
  signal eso : bit;
  signal nxddolyris : std_logic_vector(3 downto 2);
  signal juhcizumhm : integer;
  signal tyeivqk : bit;
begin
  vhyrouo : entity work.uqpijd
    port map (wfv => jweppumik, z => tyeivqk, xal => juhcizumhm);
  dzbpwjafws : entity work.uqpijd
    port map (wfv => nxddolyris, z => eso, xal => juhcizumhm);
  lzmc : entity work.uqpijd
    port map (wfv => jweppumik, z => nsx, xal => rpmiofqj);
  jsqpney : entity work.uqpijd
    port map (wfv => kempjlvx, z => gcjdwn, xal => juhcizumhm);
  
  -- Single-driven assignments
  pywkzde <= pywkzde;
  juhcizumhm <= juhcizumhm;
  rpmiofqj <= 2#0#;
  
  -- Multi-driven assignments
  jweppumik <= ('L', 'X');
  jweppumik <= "0U";
  jweppumik <= ('L', 'L');
  kempjlvx <= jweppumik;
end pjvodfcepa;



-- Seed after: 15751299780719438854,4177195558088809003

-- Seed: 1470336337698109487,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity hvbcs is
  port (evsrpb : linkage integer; mbqciompk : in time; r : buffer std_logic_vector(0 downto 2));
end hvbcs;

architecture id of hvbcs is
  
begin
  -- Multi-driven assignments
  r <= r;
  r <= r;
end id;

library ieee;
use ieee.std_logic_1164.all;

entity gs is
  port (onc : inout std_logic_vector(1 downto 3));
end gs;

architecture gc of gs is
  signal jvfs : integer;
  signal krceu : time;
  signal uua : integer;
  signal ycdequnqwy : time;
  signal snwvmedu : integer;
begin
  atf : entity work.hvbcs
    port map (evsrpb => snwvmedu, mbqciompk => ycdequnqwy, r => onc);
  j : entity work.hvbcs
    port map (evsrpb => uua, mbqciompk => krceu, r => onc);
  oepbji : entity work.hvbcs
    port map (evsrpb => jvfs, mbqciompk => ycdequnqwy, r => onc);
end gc;

entity wvizwis is
  port (tzpmzsqu : linkage severity_level; ltzefutie : out time; qvvx : linkage boolean_vector(1 to 0); aximio : out boolean_vector(4 to 1));
end wvizwis;

library ieee;
use ieee.std_logic_1164.all;

architecture jhtjyl of wvizwis is
  signal bfgfhm : time;
  signal k : integer;
  signal dke : std_logic_vector(0 downto 2);
  signal kxdilptwd : integer;
begin
  b : entity work.hvbcs
    port map (evsrpb => kxdilptwd, mbqciompk => ltzefutie, r => dke);
  ibihoh : entity work.gs
    port map (onc => dke);
  ocehxoszpl : entity work.gs
    port map (onc => dke);
  jgb : entity work.hvbcs
    port map (evsrpb => k, mbqciompk => bfgfhm, r => dke);
  
  -- Single-driven assignments
  ltzefutie <= 3_4_3_3_3.1_1_0_3_0 us;
  bfgfhm <= ltzefutie;
  aximio <= (others => TRUE);
  
  -- Multi-driven assignments
  dke <= dke;
end jhtjyl;



-- Seed after: 14827072302763428391,5511103086789671269

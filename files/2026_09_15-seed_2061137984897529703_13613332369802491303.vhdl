-- Seed: 2061137984897529703,13613332369802491303

entity jrwh is
  port (wpwwk : inout integer_vector(2 downto 3));
end jrwh;

architecture vj of jrwh is
  
begin
  -- Single-driven assignments
  wpwwk <= (others => 0);
end vj;

library ieee;
use ieee.std_logic_1164.all;

entity huzihkuv is
  port (e : out boolean; rpknesqu : buffer integer; xgyar : in std_logic_vector(0 downto 1); petwhhip : out std_logic);
end huzihkuv;

architecture aphab of huzihkuv is
  signal iv : integer_vector(2 downto 3);
  signal pmexmwyzfy : integer_vector(2 downto 3);
  signal svzvsxhkiz : integer_vector(2 downto 3);
  signal co : integer_vector(2 downto 3);
begin
  ueg : entity work.jrwh
    port map (wpwwk => co);
  uhaoaep : entity work.jrwh
    port map (wpwwk => svzvsxhkiz);
  r : entity work.jrwh
    port map (wpwwk => pmexmwyzfy);
  ocyj : entity work.jrwh
    port map (wpwwk => iv);
  
  -- Single-driven assignments
  rpknesqu <= 2#0_0_1#;
  
  -- Multi-driven assignments
  petwhhip <= petwhhip;
end aphab;



-- Seed after: 14308715518356292008,13613332369802491303

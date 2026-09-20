-- Seed: 1983032984400862087,18037650846010261179

entity ujazysbgkb is
  port (darbvl : out real; jebftlsb : inout integer; grazkt : linkage bit_vector(4 to 1));
end ujazysbgkb;

architecture c of ujazysbgkb is
  
begin
  -- Single-driven assignments
  jebftlsb <= 303;
  darbvl <= 1_3_2_2.13401;
end c;

library ieee;
use ieee.std_logic_1164.all;

entity ivswutyna is
  port (tabvguy : out bit_vector(4 to 0); s : in std_logic; gmhthdcnr : out boolean_vector(3 downto 2); eqhllbcyrg : buffer std_logic_vector(3 to 3));
end ivswutyna;

architecture scjkm of ivswutyna is
  signal eddxrdw : integer;
  signal bqf : real;
  signal sxodyqxs : bit_vector(4 to 1);
  signal ryzstrv : integer;
  signal h : real;
  signal gogx : bit_vector(4 to 1);
  signal nyh : integer;
  signal ujmeawq : real;
begin
  xtjdpidma : entity work.ujazysbgkb
    port map (darbvl => ujmeawq, jebftlsb => nyh, grazkt => gogx);
  wprzf : entity work.ujazysbgkb
    port map (darbvl => h, jebftlsb => ryzstrv, grazkt => sxodyqxs);
  wadqrtbuav : entity work.ujazysbgkb
    port map (darbvl => bqf, jebftlsb => eddxrdw, grazkt => tabvguy);
  
  -- Single-driven assignments
  gmhthdcnr <= (FALSE, TRUE);
  
  -- Multi-driven assignments
  eqhllbcyrg <= (others => 'L');
  eqhllbcyrg <= "X";
  eqhllbcyrg <= eqhllbcyrg;
end scjkm;



-- Seed after: 16933628165395826790,18037650846010261179

-- Seed: 16493530656610202197,15300320181035395489

entity r is
  port (zjbpnn : linkage time; yamrbfqpnp : linkage character);
end r;

architecture rmwzvrrvp of r is
  
begin
  
end rmwzvrrvp;

entity jmh is
  port (p : out boolean_vector(0 downto 3); xgupcwluq : out real; t : in time);
end jmh;

architecture h of jmh is
  signal a : character;
  signal appkiw : time;
  signal qfpjy : character;
  signal wean : time;
  signal gsfyamszi : character;
  signal acrm : time;
begin
  hgw : entity work.r
    port map (zjbpnn => acrm, yamrbfqpnp => gsfyamszi);
  serhicax : entity work.r
    port map (zjbpnn => wean, yamrbfqpnp => qfpjy);
  ysevge : entity work.r
    port map (zjbpnn => appkiw, yamrbfqpnp => a);
  
  -- Single-driven assignments
  p <= (others => TRUE);
  xgupcwluq <= 1_1_1_2_1.4_4_0_3_4;
end h;

entity bydbpcy is
  port (uqshh : linkage string(3 to 2));
end bydbpcy;

architecture l of bydbpcy is
  
begin
  
end l;

library ieee;
use ieee.std_logic_1164.all;

entity vubfog is
  port (nl : buffer std_logic_vector(0 downto 0); sczg : linkage time);
end vubfog;

architecture u of vubfog is
  signal voqlujty : character;
  signal v : time;
  signal ozpmxhy : character;
  signal utc : time;
  signal w : real;
  signal kwduju : boolean_vector(0 downto 3);
begin
  mcgb : entity work.jmh
    port map (p => kwduju, xgupcwluq => w, t => utc);
  meex : entity work.r
    port map (zjbpnn => utc, yamrbfqpnp => ozpmxhy);
  cjofii : entity work.r
    port map (zjbpnn => v, yamrbfqpnp => voqlujty);
  
  -- Multi-driven assignments
  nl <= (others => '1');
  nl <= "X";
  nl <= "H";
end u;



-- Seed after: 8486861749696704717,15300320181035395489

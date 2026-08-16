-- Seed: 17452284719180671132,13857275728440271305

entity q is
  port (eywdfvc : inout real; phltdz : out real; a : in time; wymgdcgezi : inout integer_vector(4 to 3));
end q;

architecture dsuaicwxb of q is
  
begin
  
end dsuaicwxb;

entity mkbpsyz is
  port (ipd : linkage integer_vector(1 downto 3));
end mkbpsyz;

architecture pngsynsciq of mkbpsyz is
  signal hkbx : integer_vector(4 to 3);
  signal jwycu : real;
  signal eaognenw : real;
  signal h : integer_vector(4 to 3);
  signal owpdbzmpl : real;
  signal f : real;
  signal fbcxwhq : integer_vector(4 to 3);
  signal uelobsd : time;
  signal hlkmoawmnc : real;
  signal zvix : real;
  signal j : integer_vector(4 to 3);
  signal thjfevv : time;
  signal banayekg : real;
  signal oqlyuntyrw : real;
begin
  y : entity work.q
    port map (eywdfvc => oqlyuntyrw, phltdz => banayekg, a => thjfevv, wymgdcgezi => j);
  gpds : entity work.q
    port map (eywdfvc => zvix, phltdz => hlkmoawmnc, a => uelobsd, wymgdcgezi => fbcxwhq);
  viubvrv : entity work.q
    port map (eywdfvc => f, phltdz => owpdbzmpl, a => thjfevv, wymgdcgezi => h);
  wqbzh : entity work.q
    port map (eywdfvc => eaognenw, phltdz => jwycu, a => uelobsd, wymgdcgezi => hkbx);
end pngsynsciq;

entity svdyd is
  port (dyrxze : in bit);
end svdyd;

architecture tyzlws of svdyd is
  signal rfayd : integer_vector(4 to 3);
  signal dgfumtgjh : time;
  signal uprfwcbp : real;
  signal tpsii : real;
  signal rgi : integer_vector(1 downto 3);
begin
  cquznp : entity work.mkbpsyz
    port map (ipd => rgi);
  z : entity work.q
    port map (eywdfvc => tpsii, phltdz => uprfwcbp, a => dgfumtgjh, wymgdcgezi => rfayd);
  
  -- Single-driven assignments
  dgfumtgjh <= dgfumtgjh;
end tyzlws;

library ieee;
use ieee.std_logic_1164.all;

entity iiscsew is
  port (udnocoip : buffer std_logic; k : linkage character; dijozjyhy : buffer time; v : buffer integer);
end iiscsew;

architecture un of iiscsew is
  signal qodjbcm : bit;
  signal iwqxwtpwtq : integer_vector(4 to 3);
  signal jevv : time;
  signal ajadafgyr : real;
  signal silrdila : real;
  signal jgcvc : integer_vector(4 to 3);
  signal kwcaix : real;
  signal fx : real;
  signal bfuvlk : integer_vector(4 to 3);
  signal gqo : time;
  signal xpler : real;
  signal cxjol : real;
begin
  fsdenv : entity work.q
    port map (eywdfvc => cxjol, phltdz => xpler, a => gqo, wymgdcgezi => bfuvlk);
  ismcilxyz : entity work.q
    port map (eywdfvc => fx, phltdz => kwcaix, a => dijozjyhy, wymgdcgezi => jgcvc);
  xoujkwf : entity work.q
    port map (eywdfvc => silrdila, phltdz => ajadafgyr, a => jevv, wymgdcgezi => iwqxwtpwtq);
  tqs : entity work.svdyd
    port map (dyrxze => qodjbcm);
  
  -- Single-driven assignments
  v <= 4_3_0_2;
  qodjbcm <= '0';
  jevv <= 2#10011# ns;
  dijozjyhy <= 1 hr;
  gqo <= gqo;
  
  -- Multi-driven assignments
  udnocoip <= 'H';
  udnocoip <= udnocoip;
  udnocoip <= 'U';
  udnocoip <= udnocoip;
end un;



-- Seed after: 6246242500442883816,13857275728440271305

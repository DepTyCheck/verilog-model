-- Seed: 10318281149413273353,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity lq is
  port (vrzo : out real; bnf : buffer real_vector(2 to 2); coun : buffer integer; tkxymfxjt : out std_logic_vector(4 to 3));
end lq;

architecture pmet of lq is
  
begin
  -- Multi-driven assignments
  tkxymfxjt <= (others => '0');
  tkxymfxjt <= "";
  tkxymfxjt <= "";
  tkxymfxjt <= "";
end pmet;

library ieee;
use ieee.std_logic_1164.all;

entity kq is
  port (quwiqfkoq : in real; sifdmmq : buffer std_logic_vector(3 to 1); yqv : buffer std_logic);
end kq;

library ieee;
use ieee.std_logic_1164.all;

architecture vy of kq is
  signal clolzm : std_logic_vector(4 to 3);
  signal icnkdi : integer;
  signal ycead : real_vector(2 to 2);
  signal cluatruyr : real;
  signal xhpbeowwbd : std_logic_vector(4 to 3);
  signal invr : integer;
  signal dgavsy : real_vector(2 to 2);
  signal jlio : real;
begin
  tvegjqrklt : entity work.lq
    port map (vrzo => jlio, bnf => dgavsy, coun => invr, tkxymfxjt => xhpbeowwbd);
  yaqez : entity work.lq
    port map (vrzo => cluatruyr, bnf => ycead, coun => icnkdi, tkxymfxjt => clolzm);
  
  -- Multi-driven assignments
  clolzm <= (others => '0');
  sifdmmq <= sifdmmq;
  clolzm <= "";
end vy;

entity chdcwgau is
  port (nqltepgg : linkage integer);
end chdcwgau;

library ieee;
use ieee.std_logic_1164.all;

architecture bkxyaghga of chdcwgau is
  signal tyjdpwnc : std_logic;
  signal vcfeov : real;
  signal wsuwkr : integer;
  signal waiqxpemz : real_vector(2 to 2);
  signal lw : real;
  signal qqbeowb : std_logic_vector(3 to 1);
  signal ivohunnhm : integer;
  signal bjralj : real_vector(2 to 2);
  signal vwsvklls : real;
begin
  ibkufhqf : entity work.lq
    port map (vrzo => vwsvklls, bnf => bjralj, coun => ivohunnhm, tkxymfxjt => qqbeowb);
  jatvrkh : entity work.lq
    port map (vrzo => lw, bnf => waiqxpemz, coun => wsuwkr, tkxymfxjt => qqbeowb);
  pdeikoe : entity work.kq
    port map (quwiqfkoq => vcfeov, sifdmmq => qqbeowb, yqv => tyjdpwnc);
  
  -- Single-driven assignments
  vcfeov <= vwsvklls;
  
  -- Multi-driven assignments
  tyjdpwnc <= tyjdpwnc;
  qqbeowb <= "";
  qqbeowb <= qqbeowb;
  qqbeowb <= qqbeowb;
end bkxyaghga;



-- Seed after: 5793365810571030449,662889661651915549

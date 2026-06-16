-- Seed: 6791742594669870465,5472058987609252853

entity ehy is
  port (vf : linkage boolean_vector(4 downto 4); whjwunxwes : buffer severity_level);
end ehy;

architecture pbopcmq of ehy is
  
begin
  -- Single-driven assignments
  whjwunxwes <= ERROR;
end pbopcmq;

entity pkacmkbm is
  port (zo : out time; ziz : inout time; q : buffer severity_level);
end pkacmkbm;

architecture xrsujkhd of pkacmkbm is
  signal wfcr : boolean_vector(4 downto 4);
  signal hckbydux : severity_level;
  signal xflcukt : boolean_vector(4 downto 4);
begin
  kbjrrhec : entity work.ehy
    port map (vf => xflcukt, whjwunxwes => hckbydux);
  cbeks : entity work.ehy
    port map (vf => wfcr, whjwunxwes => q);
end xrsujkhd;

library ieee;
use ieee.std_logic_1164.all;

entity ix is
  port (xyrucgu : in std_logic_vector(4 to 4); yiknjmioe : in time; edymazges : buffer integer; c : buffer severity_level);
end ix;

architecture bkbtbjir of ix is
  signal gtjuqoud : boolean_vector(4 downto 4);
begin
  p : entity work.ehy
    port map (vf => gtjuqoud, whjwunxwes => c);
  
  -- Single-driven assignments
  edymazges <= 2#1#;
end bkbtbjir;

library ieee;
use ieee.std_logic_1164.all;

entity fgoizukwpa is
  port (whbqgi : in std_logic; vj : buffer std_logic);
end fgoizukwpa;

library ieee;
use ieee.std_logic_1164.all;

architecture jvsj of fgoizukwpa is
  signal qmyk : severity_level;
  signal gbsy : integer;
  signal gg : std_logic_vector(4 to 4);
  signal aay : severity_level;
  signal thpqn : boolean_vector(4 downto 4);
  signal ofwnvcyb : severity_level;
  signal rhjgezylyb : boolean_vector(4 downto 4);
  signal wxjd : severity_level;
  signal rqhoaflwu : time;
  signal zjz : time;
begin
  gux : entity work.pkacmkbm
    port map (zo => zjz, ziz => rqhoaflwu, q => wxjd);
  qyhheabdp : entity work.ehy
    port map (vf => rhjgezylyb, whjwunxwes => ofwnvcyb);
  zpd : entity work.ehy
    port map (vf => thpqn, whjwunxwes => aay);
  q : entity work.ix
    port map (xyrucgu => gg, yiknjmioe => zjz, edymazges => gbsy, c => qmyk);
  
  -- Multi-driven assignments
  vj <= 'U';
  vj <= '-';
  gg <= "0";
end jvsj;



-- Seed after: 11714557692527921242,5472058987609252853

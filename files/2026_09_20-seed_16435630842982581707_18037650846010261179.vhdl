-- Seed: 16435630842982581707,18037650846010261179

entity qllbqmqvy is
  port (kfbgiqazx : buffer boolean_vector(1 downto 1); gw : in time);
end qllbqmqvy;

architecture wlpvjkiyxe of qllbqmqvy is
  
begin
  -- Single-driven assignments
  kfbgiqazx <= (others => TRUE);
end wlpvjkiyxe;

library ieee;
use ieee.std_logic_1164.all;

entity rvy is
  port (tml : out std_logic_vector(0 downto 4); lsq : buffer integer; ebsslvv : linkage std_logic);
end rvy;

architecture scw of rvy is
  signal htmpty : boolean_vector(1 downto 1);
  signal ztprblznfe : boolean_vector(1 downto 1);
  signal nqfwlorl : boolean_vector(1 downto 1);
  signal cwfj : time;
  signal lmkslxkqsy : boolean_vector(1 downto 1);
begin
  ctvio : entity work.qllbqmqvy
    port map (kfbgiqazx => lmkslxkqsy, gw => cwfj);
  mpqufmspt : entity work.qllbqmqvy
    port map (kfbgiqazx => nqfwlorl, gw => cwfj);
  wiowykc : entity work.qllbqmqvy
    port map (kfbgiqazx => ztprblznfe, gw => cwfj);
  zubu : entity work.qllbqmqvy
    port map (kfbgiqazx => htmpty, gw => cwfj);
  
  -- Single-driven assignments
  lsq <= 40;
  cwfj <= 16#9.BD# ps;
  
  -- Multi-driven assignments
  tml <= tml;
  tml <= tml;
  tml <= (others => '0');
end scw;

library ieee;
use ieee.std_logic_1164.all;

entity ebycyzkvx is
  port (inzgodq : buffer std_logic_vector(0 to 3); qpggvoy : out real; muw : buffer std_logic);
end ebycyzkvx;

architecture vtvging of ebycyzkvx is
  
begin
  
end vtvging;

entity o is
  port (jruhajsacj : inout bit);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture evhvtcuetx of o is
  signal zh : time;
  signal e : boolean_vector(1 downto 1);
  signal mxrz : std_logic;
  signal eyp : real;
  signal hsltpdc : std_logic_vector(0 to 3);
begin
  suvuj : entity work.ebycyzkvx
    port map (inzgodq => hsltpdc, qpggvoy => eyp, muw => mxrz);
  dc : entity work.qllbqmqvy
    port map (kfbgiqazx => e, gw => zh);
  
  -- Single-driven assignments
  jruhajsacj <= '0';
  zh <= 4_0 ms;
  
  -- Multi-driven assignments
  hsltpdc <= hsltpdc;
  mxrz <= 'U';
  hsltpdc <= hsltpdc;
end evhvtcuetx;



-- Seed after: 6822802894563693359,18037650846010261179

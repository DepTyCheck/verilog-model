-- Seed: 984470672246586493,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity masvfw is
  port (qtwsn : inout time; xulu : inout real_vector(0 downto 4); oyexzwymct : in std_logic_vector(4 to 0));
end masvfw;

architecture gwkiyiko of masvfw is
  
begin
  -- Single-driven assignments
  xulu <= xulu;
end gwkiyiko;

entity unmn is
  port (osxfk : linkage real; foduoiept : buffer time; rnllrqx : buffer time; uiobmebrih : out boolean_vector(0 downto 0));
end unmn;

library ieee;
use ieee.std_logic_1164.all;

architecture wse of unmn is
  signal zzkxd : std_logic_vector(4 to 0);
  signal tbmgnye : real_vector(0 downto 4);
  signal uk : time;
  signal b : std_logic_vector(4 to 0);
  signal gq : real_vector(0 downto 4);
  signal czw : time;
  signal grhivtu : std_logic_vector(4 to 0);
  signal bzbioq : real_vector(0 downto 4);
begin
  ypvcanze : entity work.masvfw
    port map (qtwsn => rnllrqx, xulu => bzbioq, oyexzwymct => grhivtu);
  tc : entity work.masvfw
    port map (qtwsn => czw, xulu => gq, oyexzwymct => b);
  takxxrb : entity work.masvfw
    port map (qtwsn => uk, xulu => tbmgnye, oyexzwymct => zzkxd);
  
  -- Single-driven assignments
  uiobmebrih <= (others => FALSE);
end wse;



-- Seed after: 7257839444373271622,6377581586043967497

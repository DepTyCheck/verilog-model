-- Seed: 693474522938223639,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity qoz is
  port (vq : inout std_logic; mxdaeaxmt : out character; qtpmtfi : in boolean_vector(1 to 3));
end qoz;

architecture fqip of qoz is
  
begin
  -- Single-driven assignments
  mxdaeaxmt <= 'i';
end fqip;

library ieee;
use ieee.std_logic_1164.all;

entity gqpqs is
  port (voual : linkage std_logic_vector(0 to 0); xxnih : linkage integer_vector(1 downto 3));
end gqpqs;

library ieee;
use ieee.std_logic_1164.all;

architecture oqlrxkal of gqpqs is
  signal fqaqwe : boolean_vector(1 to 3);
  signal nbhcdezxl : character;
  signal ijbdajulj : character;
  signal zybv : std_logic;
  signal cpfmk : character;
  signal hutvsxsnt : std_logic;
  signal nghdbdd : boolean_vector(1 to 3);
  signal awtqninwt : character;
  signal yoiuuw : std_logic;
begin
  gozvvtb : entity work.qoz
    port map (vq => yoiuuw, mxdaeaxmt => awtqninwt, qtpmtfi => nghdbdd);
  yybjozxz : entity work.qoz
    port map (vq => hutvsxsnt, mxdaeaxmt => cpfmk, qtpmtfi => nghdbdd);
  kghskgjaez : entity work.qoz
    port map (vq => zybv, mxdaeaxmt => ijbdajulj, qtpmtfi => nghdbdd);
  u : entity work.qoz
    port map (vq => yoiuuw, mxdaeaxmt => nbhcdezxl, qtpmtfi => fqaqwe);
  
  -- Single-driven assignments
  nghdbdd <= (FALSE, TRUE, TRUE);
  fqaqwe <= (FALSE, TRUE, FALSE);
  
  -- Multi-driven assignments
  hutvsxsnt <= 'W';
  zybv <= 'X';
  hutvsxsnt <= 'Z';
end oqlrxkal;



-- Seed after: 3542270703618592679,5472058987609252853

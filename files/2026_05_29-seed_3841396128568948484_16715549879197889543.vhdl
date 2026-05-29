-- Seed: 3841396128568948484,16715549879197889543

library ieee;
use ieee.std_logic_1164.all;

entity tjxv is
  port (hbkgmudb : in std_logic; wsdhtbsx : out real; cfkwu : buffer std_logic_vector(1 downto 4); eanv : in integer_vector(2 downto 1));
end tjxv;



architecture gdhew of tjxv is
  
begin
  
end gdhew;

library ieee;
use ieee.std_logic_1164.all;

entity gz is
  port (qc : in time; pjggjlyd : inout real_vector(2 downto 1); dcopanvl : buffer severity_level; emrhp : in std_logic_vector(3 downto 3));
end gz;

library ieee;
use ieee.std_logic_1164.all;

architecture kzileas of gz is
  signal jwjwqsouvy : std_logic_vector(1 downto 4);
  signal cbsykybh : real;
  signal o : std_logic_vector(1 downto 4);
  signal iexl : real;
  signal zwzcu : std_logic_vector(1 downto 4);
  signal gekgdhwt : real;
  signal dqlfysekbc : integer_vector(2 downto 1);
  signal bxf : std_logic_vector(1 downto 4);
  signal auesid : real;
  signal gefkb : std_logic;
begin
  hmbybgduuv : entity work.tjxv
    port map (hbkgmudb => gefkb, wsdhtbsx => auesid, cfkwu => bxf, eanv => dqlfysekbc);
  kliklgcfdt : entity work.tjxv
    port map (hbkgmudb => gefkb, wsdhtbsx => gekgdhwt, cfkwu => zwzcu, eanv => dqlfysekbc);
  dolw : entity work.tjxv
    port map (hbkgmudb => gefkb, wsdhtbsx => iexl, cfkwu => o, eanv => dqlfysekbc);
  lxgvw : entity work.tjxv
    port map (hbkgmudb => gefkb, wsdhtbsx => cbsykybh, cfkwu => jwjwqsouvy, eanv => dqlfysekbc);
end kzileas;



entity yd is
  port (aqt : linkage time);
end yd;

library ieee;
use ieee.std_logic_1164.all;

architecture wamhv of yd is
  signal jrny : integer_vector(2 downto 1);
  signal voj : real;
  signal cavfyf : std_logic_vector(1 downto 4);
  signal jhrrh : real;
  signal xdffbqhjpl : integer_vector(2 downto 1);
  signal xsdu : real;
  signal qvyfg : std_logic;
  signal xovaytbpc : integer_vector(2 downto 1);
  signal qwqwpoiyc : std_logic_vector(1 downto 4);
  signal du : real;
  signal vklyskf : std_logic;
begin
  fiq : entity work.tjxv
    port map (hbkgmudb => vklyskf, wsdhtbsx => du, cfkwu => qwqwpoiyc, eanv => xovaytbpc);
  azdzn : entity work.tjxv
    port map (hbkgmudb => qvyfg, wsdhtbsx => xsdu, cfkwu => qwqwpoiyc, eanv => xdffbqhjpl);
  qqkvo : entity work.tjxv
    port map (hbkgmudb => vklyskf, wsdhtbsx => jhrrh, cfkwu => cavfyf, eanv => xdffbqhjpl);
  u : entity work.tjxv
    port map (hbkgmudb => vklyskf, wsdhtbsx => voj, cfkwu => cavfyf, eanv => jrny);
end wamhv;



entity ksqjkxrd is
  port (rtpejftdx : in time; vdqo : out real; bpfh : in boolean_vector(4 to 2));
end ksqjkxrd;

library ieee;
use ieee.std_logic_1164.all;

architecture hlxphz of ksqjkxrd is
  signal wrvtzty : real;
  signal cfthvlt : std_logic_vector(1 downto 4);
  signal ytjvkkwo : time;
  signal st : integer_vector(2 downto 1);
  signal h : std_logic_vector(1 downto 4);
  signal xxuwwtjcsh : real;
  signal ifwpfhlbfi : std_logic;
begin
  qnagg : entity work.tjxv
    port map (hbkgmudb => ifwpfhlbfi, wsdhtbsx => xxuwwtjcsh, cfkwu => h, eanv => st);
  dhffxjt : entity work.yd
    port map (aqt => ytjvkkwo);
  afb : entity work.tjxv
    port map (hbkgmudb => ifwpfhlbfi, wsdhtbsx => vdqo, cfkwu => cfthvlt, eanv => st);
  zbh : entity work.tjxv
    port map (hbkgmudb => ifwpfhlbfi, wsdhtbsx => wrvtzty, cfkwu => h, eanv => st);
end hlxphz;



-- Seed after: 4322025327122872560,16715549879197889543

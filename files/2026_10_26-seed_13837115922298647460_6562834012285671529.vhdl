-- Seed: 13837115922298647460,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity qt is
  port (atfwjl : inout std_logic_vector(1 downto 2); bksrdw : buffer time; yjt : linkage std_logic);
end qt;

architecture xgnwbgr of qt is
  
begin
  -- Multi-driven assignments
  atfwjl <= "";
end xgnwbgr;

entity mdia is
  port (enowwhnlzm : out boolean_vector(3 to 3));
end mdia;

library ieee;
use ieee.std_logic_1164.all;

architecture eoq of mdia is
  signal xwv : std_logic;
  signal xvml : time;
  signal tgspahahye : std_logic_vector(1 downto 2);
  signal wfw : std_logic;
  signal dyos : time;
  signal ehakoddqhv : std_logic_vector(1 downto 2);
begin
  ubiepowka : entity work.qt
    port map (atfwjl => ehakoddqhv, bksrdw => dyos, yjt => wfw);
  ltvbnxokyo : entity work.qt
    port map (atfwjl => tgspahahye, bksrdw => xvml, yjt => xwv);
  
  -- Single-driven assignments
  enowwhnlzm <= enowwhnlzm;
  
  -- Multi-driven assignments
  tgspahahye <= ehakoddqhv;
  tgspahahye <= ehakoddqhv;
end eoq;

library ieee;
use ieee.std_logic_1164.all;

entity hzojkmha is
  port (qtoqlbskn : buffer integer; fdoex : buffer std_logic; xngfv : inout time);
end hzojkmha;

library ieee;
use ieee.std_logic_1164.all;

architecture pihr of hzojkmha is
  signal ovdq : std_logic;
  signal fyhjkbvlvx : std_logic_vector(1 downto 2);
  signal e : boolean_vector(3 to 3);
  signal oodcssuah : time;
  signal aqyaz : time;
  signal traikqfrl : std_logic_vector(1 downto 2);
begin
  iocmznakm : entity work.qt
    port map (atfwjl => traikqfrl, bksrdw => aqyaz, yjt => fdoex);
  jqdbawz : entity work.qt
    port map (atfwjl => traikqfrl, bksrdw => oodcssuah, yjt => fdoex);
  f : entity work.mdia
    port map (enowwhnlzm => e);
  nlbhfnn : entity work.qt
    port map (atfwjl => fyhjkbvlvx, bksrdw => xngfv, yjt => ovdq);
  
  -- Single-driven assignments
  qtoqlbskn <= qtoqlbskn;
  
  -- Multi-driven assignments
  fdoex <= fdoex;
  ovdq <= fdoex;
  fdoex <= fdoex;
end pihr;



-- Seed after: 17890402490496077485,6562834012285671529

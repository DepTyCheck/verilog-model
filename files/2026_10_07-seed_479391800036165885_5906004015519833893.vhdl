-- Seed: 479391800036165885,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity ynqiw is
  port (i : buffer std_logic);
end ynqiw;

architecture alcrhyk of ynqiw is
  
begin
  -- Multi-driven assignments
  i <= '0';
  i <= i;
end alcrhyk;

library ieee;
use ieee.std_logic_1164.all;

entity yprplgcvz is
  port (juohji : in std_logic_vector(2 to 0));
end yprplgcvz;

library ieee;
use ieee.std_logic_1164.all;

architecture n of yprplgcvz is
  signal evzhtgmn : std_logic;
begin
  bf : entity work.ynqiw
    port map (i => evzhtgmn);
  fyvgexhdwa : entity work.ynqiw
    port map (i => evzhtgmn);
  enqyx : entity work.ynqiw
    port map (i => evzhtgmn);
  
  -- Multi-driven assignments
  evzhtgmn <= 'H';
  evzhtgmn <= evzhtgmn;
  evzhtgmn <= 'Z';
end n;

entity wanrmjbci is
  port (jgppswlvmu : inout time; mahkvz : inout integer);
end wanrmjbci;

library ieee;
use ieee.std_logic_1164.all;

architecture otlqq of wanrmjbci is
  signal jyxxbkx : std_logic;
  signal agwykjii : std_logic_vector(2 to 0);
begin
  mmkx : entity work.yprplgcvz
    port map (juohji => agwykjii);
  xkd : entity work.ynqiw
    port map (i => jyxxbkx);
  bxvqujssb : entity work.ynqiw
    port map (i => jyxxbkx);
  
  -- Single-driven assignments
  mahkvz <= mahkvz;
  jgppswlvmu <= 30403.3_0_0_2 ms;
  
  -- Multi-driven assignments
  agwykjii <= agwykjii;
  agwykjii <= agwykjii;
  jyxxbkx <= '0';
end otlqq;

entity eqpx is
  port (kdtqzu : out real; zocmbjzu : buffer real; rdacrqsds : inout time; ddhqmfbwcw : out real);
end eqpx;

library ieee;
use ieee.std_logic_1164.all;

architecture mmgji of eqpx is
  signal oqsdki : integer;
  signal npbmalwuvo : time;
  signal whhgn : std_logic_vector(2 to 0);
  signal sinvzfzilr : integer;
  signal igz : time;
  signal qxopadqgl : std_logic_vector(2 to 0);
begin
  rdyk : entity work.yprplgcvz
    port map (juohji => qxopadqgl);
  csaegke : entity work.wanrmjbci
    port map (jgppswlvmu => igz, mahkvz => sinvzfzilr);
  lajcna : entity work.yprplgcvz
    port map (juohji => whhgn);
  ullpjcyk : entity work.wanrmjbci
    port map (jgppswlvmu => npbmalwuvo, mahkvz => oqsdki);
  
  -- Single-driven assignments
  kdtqzu <= ddhqmfbwcw;
  
  -- Multi-driven assignments
  qxopadqgl <= "";
  qxopadqgl <= (others => '0');
  qxopadqgl <= "";
end mmgji;



-- Seed after: 6334451276457033243,5906004015519833893

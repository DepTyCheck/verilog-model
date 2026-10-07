-- Seed: 9109150111977371451,5906004015519833893

entity iggg is
  port (yfh : buffer time; mbhtvnpwvo : inout time);
end iggg;

architecture nqpzdj of iggg is
  
begin
  -- Single-driven assignments
  mbhtvnpwvo <= yfh;
  yfh <= 1 hr;
end nqpzdj;

library ieee;
use ieee.std_logic_1164.all;

entity heheq is
  port (ck : buffer std_logic; gkqlfk : in std_logic);
end heheq;

architecture dahda of heheq is
  signal tn : time;
  signal gchwk : time;
  signal lll : time;
  signal v : time;
  signal ruxegvikqf : time;
  signal g : time;
  signal lwobbhblfz : time;
  signal ckafqrxytn : time;
begin
  sx : entity work.iggg
    port map (yfh => ckafqrxytn, mbhtvnpwvo => lwobbhblfz);
  tvgthqmtvy : entity work.iggg
    port map (yfh => g, mbhtvnpwvo => ruxegvikqf);
  wvmcwgve : entity work.iggg
    port map (yfh => v, mbhtvnpwvo => lll);
  csnspghdv : entity work.iggg
    port map (yfh => gchwk, mbhtvnpwvo => tn);
  
  -- Multi-driven assignments
  ck <= ck;
  ck <= '-';
  ck <= gkqlfk;
end dahda;



-- Seed after: 17966719678900474325,5906004015519833893

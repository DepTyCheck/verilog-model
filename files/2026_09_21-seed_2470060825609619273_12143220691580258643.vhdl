-- Seed: 2470060825609619273,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity qnjmvd is
  port (mdzlhlchvd : buffer string(5 to 5); lktdpikrxb : in time; m : buffer std_logic_vector(4 to 2));
end qnjmvd;

architecture fa of qnjmvd is
  
begin
  -- Multi-driven assignments
  m <= m;
end fa;

library ieee;
use ieee.std_logic_1164.all;

entity kbsjms is
  port (xkjx : in character; uylecjmhi : linkage std_logic_vector(3 to 1));
end kbsjms;

library ieee;
use ieee.std_logic_1164.all;

architecture wuamk of kbsjms is
  signal lk : time;
  signal eolqtdat : string(5 to 5);
  signal tlyjtltt : string(5 to 5);
  signal ucxkipno : time;
  signal rvb : string(5 to 5);
  signal hz : std_logic_vector(4 to 2);
  signal siu : time;
  signal nrrakxp : string(5 to 5);
begin
  cfp : entity work.qnjmvd
    port map (mdzlhlchvd => nrrakxp, lktdpikrxb => siu, m => hz);
  siuy : entity work.qnjmvd
    port map (mdzlhlchvd => rvb, lktdpikrxb => ucxkipno, m => hz);
  iphpdda : entity work.qnjmvd
    port map (mdzlhlchvd => tlyjtltt, lktdpikrxb => siu, m => hz);
  rtaubqyb : entity work.qnjmvd
    port map (mdzlhlchvd => eolqtdat, lktdpikrxb => lk, m => hz);
  
  -- Single-driven assignments
  siu <= siu;
end wuamk;

entity paal is
  port (ayvywgxd : out time);
end paal;

library ieee;
use ieee.std_logic_1164.all;

architecture uoxh of paal is
  signal xxoiagxv : std_logic_vector(4 to 2);
  signal blsp : string(5 to 5);
  signal kaqc : std_logic_vector(4 to 2);
  signal vuhnmwdfm : time;
  signal snkibyykt : string(5 to 5);
begin
  jivmrlmar : entity work.qnjmvd
    port map (mdzlhlchvd => snkibyykt, lktdpikrxb => vuhnmwdfm, m => kaqc);
  tjnfnvr : entity work.qnjmvd
    port map (mdzlhlchvd => blsp, lktdpikrxb => vuhnmwdfm, m => xxoiagxv);
  
  -- Single-driven assignments
  ayvywgxd <= 0_0_4_3.3110 ns;
  vuhnmwdfm <= ayvywgxd;
  
  -- Multi-driven assignments
  kaqc <= (others => '0');
  kaqc <= "";
  kaqc <= "";
end uoxh;



-- Seed after: 9539460458578455936,12143220691580258643

-- Seed: 16821921742463497997,3042374792655995433

entity atrkbmhvxo is
  port (hxxh : out integer);
end atrkbmhvxo;

architecture bjljnih of atrkbmhvxo is
  
begin
  
end bjljnih;

entity dmthlpa is
  port (nyrgmqi : inout real; kizwf : out real_vector(2 downto 1); kyre : inout time_vector(0 to 3); jfktxgd : inout time_vector(1 downto 2));
end dmthlpa;

architecture hirxhklwjd of dmthlpa is
  signal fgh : integer;
begin
  kdkx : entity work.atrkbmhvxo
    port map (hxxh => fgh);
  
  -- Single-driven assignments
  jfktxgd <= jfktxgd;
  nyrgmqi <= nyrgmqi;
  kyre <= (16#B_2_B# ms, 2 ms, 8#0_5_0_2_5.570# ns, 32 ms);
  kizwf <= kizwf;
end hirxhklwjd;

library ieee;
use ieee.std_logic_1164.all;

entity dxeku is
  port (djimh : linkage std_logic; seqagpp : buffer bit);
end dxeku;

architecture whsv of dxeku is
  signal yxmq : integer;
  signal dwfp : integer;
  signal at : integer;
begin
  g : entity work.atrkbmhvxo
    port map (hxxh => at);
  shijyg : entity work.atrkbmhvxo
    port map (hxxh => dwfp);
  wydfkvszfl : entity work.atrkbmhvxo
    port map (hxxh => yxmq);
  
  -- Single-driven assignments
  seqagpp <= '0';
end whsv;

entity fylffbn is
  port (jfaa : inout real; vgzoqf : inout real);
end fylffbn;

architecture lyuzjxqn of fylffbn is
  
begin
  -- Single-driven assignments
  jfaa <= 8#15.01#;
  vgzoqf <= 2#0_1.1#;
end lyuzjxqn;



-- Seed after: 6746952934745929926,3042374792655995433

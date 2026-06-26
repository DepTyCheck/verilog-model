-- Seed: 6953039294533275692,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity mu is
  port (gsqzyrah : out real; iewk : inout time; ueah : inout time; jnm : buffer std_logic);
end mu;

architecture h of mu is
  
begin
  -- Single-driven assignments
  ueah <= 8#72# ns;
  gsqzyrah <= 2204.3;
  iewk <= 344 ns;
  
  -- Multi-driven assignments
  jnm <= 'H';
end h;

entity anrm is
  port (gmmkpwge : in real);
end anrm;

library ieee;
use ieee.std_logic_1164.all;

architecture lexaspxhi of anrm is
  signal lkr : std_logic;
  signal jslgcrcilt : time;
  signal ejxbrgv : time;
  signal eensbuhww : real;
  signal zwa : std_logic;
  signal foxl : time;
  signal uaqgwgsz : time;
  signal mudy : real;
  signal xsjb : std_logic;
  signal m : time;
  signal asvvjzbup : time;
  signal jymndky : real;
  signal ezqhvqcxu : std_logic;
  signal ijlxxoh : time;
  signal lhvfbsm : time;
  signal eifr : real;
begin
  vbum : entity work.mu
    port map (gsqzyrah => eifr, iewk => lhvfbsm, ueah => ijlxxoh, jnm => ezqhvqcxu);
  zqfz : entity work.mu
    port map (gsqzyrah => jymndky, iewk => asvvjzbup, ueah => m, jnm => xsjb);
  pxibbhgpfv : entity work.mu
    port map (gsqzyrah => mudy, iewk => uaqgwgsz, ueah => foxl, jnm => zwa);
  s : entity work.mu
    port map (gsqzyrah => eensbuhww, iewk => ejxbrgv, ueah => jslgcrcilt, jnm => lkr);
  
  -- Multi-driven assignments
  lkr <= '1';
  lkr <= 'L';
  zwa <= 'W';
end lexaspxhi;



-- Seed after: 15964334365882308234,12011142928354116943

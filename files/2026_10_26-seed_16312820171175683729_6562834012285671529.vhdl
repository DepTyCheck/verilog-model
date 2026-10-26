-- Seed: 16312820171175683729,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (hnfm : buffer std_logic_vector(0 to 3); pwvw : inout std_logic; dugzas : inout std_logic_vector(0 downto 3); kqg : buffer character);
end z;

architecture v of z is
  
begin
  -- Single-driven assignments
  kqg <= 'q';
  
  -- Multi-driven assignments
  pwvw <= pwvw;
  dugzas <= dugzas;
  hnfm <= ('0', 'W', 'X', 'X');
  dugzas <= (others => '0');
end v;

entity vdm is
  port (j : buffer time_vector(4 downto 2); qe : in time; eft : inout bit_vector(3 downto 1));
end vdm;

library ieee;
use ieee.std_logic_1164.all;

architecture wfmyyp of vdm is
  signal napc : character;
  signal yv : std_logic;
  signal jajpdvez : character;
  signal r : std_logic_vector(0 downto 3);
  signal dsauldy : std_logic;
  signal t : std_logic_vector(0 to 3);
  signal l : character;
  signal vkoeywlx : character;
  signal gtbgkyvufa : std_logic_vector(0 downto 3);
  signal yfcdkzqr : std_logic;
  signal ckjy : std_logic_vector(0 to 3);
begin
  o : entity work.z
    port map (hnfm => ckjy, pwvw => yfcdkzqr, dugzas => gtbgkyvufa, kqg => vkoeywlx);
  ezs : entity work.z
    port map (hnfm => ckjy, pwvw => yfcdkzqr, dugzas => gtbgkyvufa, kqg => l);
  sfprycyism : entity work.z
    port map (hnfm => t, pwvw => dsauldy, dugzas => r, kqg => jajpdvez);
  chulbqeso : entity work.z
    port map (hnfm => ckjy, pwvw => yv, dugzas => r, kqg => napc);
  
  -- Single-driven assignments
  eft <= ('0', '1', '1');
  j <= (4333 ms, 2#0.0_0_1_0# ns, 1 hr);
  
  -- Multi-driven assignments
  r <= (others => '0');
end wfmyyp;



-- Seed after: 13837115922298647460,6562834012285671529

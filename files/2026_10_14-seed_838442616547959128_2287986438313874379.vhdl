-- Seed: 838442616547959128,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity yasircds is
  port (djqg : out std_logic_vector(1 downto 0); vo : out time);
end yasircds;

architecture nblu of yasircds is
  
begin
  -- Single-driven assignments
  vo <= 2_0_2_4.4_3_3 us;
  
  -- Multi-driven assignments
  djqg <= djqg;
  djqg <= djqg;
end nblu;

library ieee;
use ieee.std_logic_1164.all;

entity ww is
  port (opumt : in std_logic_vector(2 to 1); xeotv : out character; ku : out real; xudcnr : in std_logic);
end ww;

library ieee;
use ieee.std_logic_1164.all;

architecture fpcns of ww is
  signal bufr : time;
  signal br : std_logic_vector(1 downto 0);
  signal aetp : time;
  signal kuslg : time;
  signal gvyitpysn : std_logic_vector(1 downto 0);
begin
  benskgypv : entity work.yasircds
    port map (djqg => gvyitpysn, vo => kuslg);
  gfyr : entity work.yasircds
    port map (djqg => gvyitpysn, vo => aetp);
  odzrs : entity work.yasircds
    port map (djqg => br, vo => bufr);
  
  -- Single-driven assignments
  ku <= ku;
  
  -- Multi-driven assignments
  gvyitpysn <= ('1', '1');
  gvyitpysn <= gvyitpysn;
end fpcns;



-- Seed after: 16779510648518625212,2287986438313874379

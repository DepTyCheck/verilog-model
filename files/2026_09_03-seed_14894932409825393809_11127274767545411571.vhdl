-- Seed: 14894932409825393809,11127274767545411571

entity uy is
  port (vtdchafyob : inout time_vector(1 downto 3); btl : in integer; zigdszoeuw : buffer time);
end uy;

architecture yqzbuiommz of uy is
  
begin
  -- Single-driven assignments
  vtdchafyob <= vtdchafyob;
  zigdszoeuw <= zigdszoeuw;
end yqzbuiommz;

library ieee;
use ieee.std_logic_1164.all;

entity bkea is
  port (lrc : linkage std_logic_vector(1 downto 1); iqdza : out std_logic; akb : inout time);
end bkea;

architecture tldqbfakno of bkea is
  signal inywvto : time;
  signal yfebkybadn : integer;
  signal qkmktbui : time_vector(1 downto 3);
  signal aidclt : time_vector(1 downto 3);
  signal nqsxbu : time;
  signal lxdzfmhoe : integer;
  signal itlphu : time_vector(1 downto 3);
begin
  cofrq : entity work.uy
    port map (vtdchafyob => itlphu, btl => lxdzfmhoe, zigdszoeuw => nqsxbu);
  haonz : entity work.uy
    port map (vtdchafyob => aidclt, btl => lxdzfmhoe, zigdszoeuw => akb);
  fsav : entity work.uy
    port map (vtdchafyob => qkmktbui, btl => yfebkybadn, zigdszoeuw => inywvto);
  
  -- Single-driven assignments
  lxdzfmhoe <= 3_0_3_2_3;
  yfebkybadn <= 3_1_4_3;
  
  -- Multi-driven assignments
  iqdza <= iqdza;
  iqdza <= iqdza;
  iqdza <= '0';
end tldqbfakno;

library ieee;
use ieee.std_logic_1164.all;

entity sxs is
  port (rizqeou : in bit_vector(1 downto 2); hmmuv : buffer time; sklxyr : in std_logic);
end sxs;

architecture bacmeqwseg of sxs is
  
begin
  -- Single-driven assignments
  hmmuv <= hmmuv;
end bacmeqwseg;

library ieee;
use ieee.std_logic_1164.all;

entity hkc is
  port (ibjgkcauw : in integer; yrkaamcnwf : linkage bit_vector(0 to 1); cfxu : linkage std_logic_vector(4 to 0));
end hkc;

library ieee;
use ieee.std_logic_1164.all;

architecture bhj of hkc is
  signal qnj : std_logic;
  signal soq : time;
  signal xmtctvg : bit_vector(1 downto 2);
  signal m : time;
  signal hzcfxwsliz : std_logic;
  signal wgkg : std_logic_vector(1 downto 1);
  signal kpiwunyboa : time;
  signal k : integer;
  signal yracixco : time_vector(1 downto 3);
begin
  yt : entity work.uy
    port map (vtdchafyob => yracixco, btl => k, zigdszoeuw => kpiwunyboa);
  icpaqvfgqk : entity work.bkea
    port map (lrc => wgkg, iqdza => hzcfxwsliz, akb => m);
  jb : entity work.sxs
    port map (rizqeou => xmtctvg, hmmuv => soq, sklxyr => qnj);
  
  -- Single-driven assignments
  xmtctvg <= (others => '0');
  
  -- Multi-driven assignments
  qnj <= 'X';
  wgkg <= (others => 'X');
end bhj;



-- Seed after: 4110015651421719335,11127274767545411571

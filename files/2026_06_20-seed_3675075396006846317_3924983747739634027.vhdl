-- Seed: 3675075396006846317,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity pucgwhxj is
  port (hou : in std_logic; ozpnc : inout real_vector(0 to 1); hxwef : inout real_vector(3 to 2));
end pucgwhxj;

architecture ykpusqv of pucgwhxj is
  
begin
  -- Single-driven assignments
  hxwef <= (others => 0.0);
end ykpusqv;

entity mqu is
  port (lrlbh : out character);
end mqu;

library ieee;
use ieee.std_logic_1164.all;

architecture nbtphe of mqu is
  signal tljbkyz : real_vector(3 to 2);
  signal rcwyqpg : real_vector(0 to 1);
  signal rkgr : real_vector(3 to 2);
  signal ckkojaaeeg : real_vector(0 to 1);
  signal yojd : std_logic;
  signal hstgxg : real_vector(3 to 2);
  signal mzfk : real_vector(0 to 1);
  signal xir : std_logic;
  signal wmt : real_vector(3 to 2);
  signal vqynyeylp : real_vector(0 to 1);
  signal wxrqirsdik : std_logic;
begin
  gygts : entity work.pucgwhxj
    port map (hou => wxrqirsdik, ozpnc => vqynyeylp, hxwef => wmt);
  cam : entity work.pucgwhxj
    port map (hou => xir, ozpnc => mzfk, hxwef => hstgxg);
  jpcpmq : entity work.pucgwhxj
    port map (hou => yojd, ozpnc => ckkojaaeeg, hxwef => rkgr);
  iim : entity work.pucgwhxj
    port map (hou => wxrqirsdik, ozpnc => rcwyqpg, hxwef => tljbkyz);
  
  -- Multi-driven assignments
  wxrqirsdik <= '0';
  wxrqirsdik <= '-';
end nbtphe;



-- Seed after: 14194533189348375018,3924983747739634027

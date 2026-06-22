-- Seed: 16936042603125077033,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity vpu is
  port (ybfce : out std_logic_vector(3 downto 2); foqxy : in std_logic; p : inout bit_vector(3 to 3));
end vpu;

architecture lmvfint of vpu is
  
begin
  -- Single-driven assignments
  p <= (others => '1');
  
  -- Multi-driven assignments
  ybfce <= "HX";
  ybfce <= ('0', 'H');
  ybfce <= "0W";
  ybfce <= "L0";
end lmvfint;

entity pjyn is
  port (baychsb : linkage integer; aggchl : inout time; sm : linkage integer);
end pjyn;

library ieee;
use ieee.std_logic_1164.all;

architecture ygyealhmb of pjyn is
  signal ir : bit_vector(3 to 3);
  signal zgzydrlha : bit_vector(3 to 3);
  signal mxutbbs : std_logic_vector(3 downto 2);
  signal yp : bit_vector(3 to 3);
  signal vjd : std_logic;
  signal xqlqjki : std_logic_vector(3 downto 2);
begin
  fx : entity work.vpu
    port map (ybfce => xqlqjki, foqxy => vjd, p => yp);
  ie : entity work.vpu
    port map (ybfce => mxutbbs, foqxy => vjd, p => zgzydrlha);
  elxl : entity work.vpu
    port map (ybfce => xqlqjki, foqxy => vjd, p => ir);
  
  -- Single-driven assignments
  aggchl <= 1_4_1_3.0 us;
  
  -- Multi-driven assignments
  xqlqjki <= "ZL";
  xqlqjki <= "LU";
  mxutbbs <= "LL";
  xqlqjki <= "0W";
end ygyealhmb;



-- Seed after: 3128528962494111687,13479070923501788437

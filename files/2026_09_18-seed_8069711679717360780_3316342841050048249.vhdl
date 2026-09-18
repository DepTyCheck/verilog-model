-- Seed: 8069711679717360780,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity detzduyc is
  port (dnf : in time; aldov : in std_logic_vector(1 to 2); bnixbjldr : out std_logic; yunsmc : in std_logic);
end detzduyc;

architecture qnnvqnaseo of detzduyc is
  
begin
  -- Multi-driven assignments
  bnixbjldr <= yunsmc;
  bnixbjldr <= yunsmc;
end qnnvqnaseo;

entity mt is
  port (ix : in real);
end mt;

library ieee;
use ieee.std_logic_1164.all;

architecture dzmltny of mt is
  signal pnfiiffb : std_logic;
  signal vegozrg : std_logic;
  signal khluv : std_logic_vector(1 to 2);
  signal whfgt : time;
  signal lkprxnk : std_logic;
  signal kicey : std_logic_vector(1 to 2);
  signal eslxnlrsy : time;
begin
  msmear : entity work.detzduyc
    port map (dnf => eslxnlrsy, aldov => kicey, bnixbjldr => lkprxnk, yunsmc => lkprxnk);
  tfubgjg : entity work.detzduyc
    port map (dnf => whfgt, aldov => khluv, bnixbjldr => vegozrg, yunsmc => pnfiiffb);
  
  -- Single-driven assignments
  whfgt <= eslxnlrsy;
  eslxnlrsy <= 1 min;
  
  -- Multi-driven assignments
  vegozrg <= lkprxnk;
  vegozrg <= '1';
  lkprxnk <= lkprxnk;
  khluv <= "-L";
end dzmltny;



-- Seed after: 4630389603263345458,3316342841050048249

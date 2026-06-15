-- Seed: 12676774055223449841,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (bckfsnblkc : linkage std_logic_vector(3 downto 0));
end l;

architecture cgvxedluox of l is
  
begin
  
end cgvxedluox;

entity tnc is
  port (ujgshxgr : inout bit_vector(2 downto 3));
end tnc;

architecture digoia of tnc is
  
begin
  -- Single-driven assignments
  ujgshxgr <= (others => '0');
end digoia;

entity yxyahcwj is
  port (pzonvx : in integer; uiqhkttho : out boolean_vector(1 to 1));
end yxyahcwj;

library ieee;
use ieee.std_logic_1164.all;

architecture te of yxyahcwj is
  signal pmrri : std_logic_vector(3 downto 0);
begin
  xllfjuhoe : entity work.l
    port map (bckfsnblkc => pmrri);
  
  -- Single-driven assignments
  uiqhkttho <= (others => FALSE);
  
  -- Multi-driven assignments
  pmrri <= "ZL0L";
  pmrri <= ('H', 'X', 'H', '-');
end te;

library ieee;
use ieee.std_logic_1164.all;

entity imzb is
  port (ungcfcxfl : out real; fidbnam : buffer std_logic_vector(3 to 1));
end imzb;

library ieee;
use ieee.std_logic_1164.all;

architecture kep of imzb is
  signal cb : boolean_vector(1 to 1);
  signal kaneuwhdq : integer;
  signal mxmdiy : std_logic_vector(3 downto 0);
begin
  bbzn : entity work.l
    port map (bckfsnblkc => mxmdiy);
  eceson : entity work.yxyahcwj
    port map (pzonvx => kaneuwhdq, uiqhkttho => cb);
  
  -- Single-driven assignments
  kaneuwhdq <= 16#10FB#;
  ungcfcxfl <= 2#1_0_0_1_1.1_1_1_1#;
  
  -- Multi-driven assignments
  fidbnam <= (others => '0');
  mxmdiy <= ('X', '0', 'W', 'Z');
  fidbnam <= (others => '0');
  mxmdiy <= ('W', 'U', 'L', 'W');
end kep;



-- Seed after: 13772571813793441714,15300320181035395489

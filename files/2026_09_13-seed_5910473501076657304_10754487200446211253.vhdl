-- Seed: 5910473501076657304,10754487200446211253

entity x is
  port (m : buffer time; rqysp : buffer severity_level);
end x;

architecture cbnze of x is
  
begin
  -- Single-driven assignments
  rqysp <= NOTE;
  m <= m;
end cbnze;

library ieee;
use ieee.std_logic_1164.all;

entity hzplzkvrxf is
  port (rqwtjdp : in std_logic_vector(2 to 2); myqzc : in std_logic; cdk : inout std_logic_vector(1 to 4));
end hzplzkvrxf;

architecture kpnsvofo of hzplzkvrxf is
  signal octrkabutr : severity_level;
  signal fogwdu : time;
  signal snxoumgdhd : severity_level;
  signal bjbfl : time;
begin
  tvxasdp : entity work.x
    port map (m => bjbfl, rqysp => snxoumgdhd);
  k : entity work.x
    port map (m => fogwdu, rqysp => octrkabutr);
  
  -- Multi-driven assignments
  cdk <= "1-1H";
end kpnsvofo;

library ieee;
use ieee.std_logic_1164.all;

entity aixoezpnn is
  port (cbtia : buffer time; seiuptp : buffer std_logic; xiwidijqb : inout std_logic_vector(3 downto 1));
end aixoezpnn;

architecture fb of aixoezpnn is
  signal spjrdbejbe : severity_level;
  signal gccmc : severity_level;
  signal vrmrekzpb : time;
  signal lln : severity_level;
  signal tcenhrrmai : time;
begin
  tgdm : entity work.x
    port map (m => tcenhrrmai, rqysp => lln);
  wgsivpeqh : entity work.x
    port map (m => vrmrekzpb, rqysp => gccmc);
  wxfoznh : entity work.x
    port map (m => cbtia, rqysp => spjrdbejbe);
  
  -- Multi-driven assignments
  seiuptp <= seiuptp;
  xiwidijqb <= xiwidijqb;
  xiwidijqb <= ('W', 'U', 'U');
end fb;



-- Seed after: 1098877347151774896,10754487200446211253

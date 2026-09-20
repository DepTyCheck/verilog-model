-- Seed: 6247806471813047445,18037650846010261179

entity zhyqn is
  port (wvsfwsgr : linkage severity_level; hyhxvqyz : inout character);
end zhyqn;

architecture hhlkdgczbf of zhyqn is
  
begin
  -- Single-driven assignments
  hyhxvqyz <= 'e';
end hhlkdgczbf;

entity nlappj is
  port (a : out severity_level);
end nlappj;

architecture dgaishyid of nlappj is
  
begin
  -- Single-driven assignments
  a <= ERROR;
end dgaishyid;

entity qywudxc is
  port (dy : linkage integer_vector(2 downto 0));
end qywudxc;

architecture neu of qywudxc is
  signal ntciqsvpk : severity_level;
  signal nyavpbncys : severity_level;
begin
  igw : entity work.nlappj
    port map (a => nyavpbncys);
  gjgttsxho : entity work.nlappj
    port map (a => ntciqsvpk);
end neu;

library ieee;
use ieee.std_logic_1164.all;

entity uy is
  port (pnifvufy : out std_logic);
end uy;

architecture ot of uy is
  signal q : integer_vector(2 downto 0);
  signal nllrxgob : character;
  signal ty : severity_level;
begin
  sor : entity work.zhyqn
    port map (wvsfwsgr => ty, hyhxvqyz => nllrxgob);
  vgcvxqhe : entity work.qywudxc
    port map (dy => q);
end ot;



-- Seed after: 1336633759793179466,18037650846010261179

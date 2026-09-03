-- Seed: 14191945434829122196,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity psdin is
  port (vr : inout bit; nkub : out std_logic_vector(3 downto 2); bpqnkvli : buffer real);
end psdin;

architecture ulwd of psdin is
  
begin
  
end ulwd;

library ieee;
use ieee.std_logic_1164.all;

entity nxafj is
  port (xjsytfu : inout std_logic; odzvi : linkage time; c : in real; guixqnng : buffer std_logic);
end nxafj;

library ieee;
use ieee.std_logic_1164.all;

architecture irxgossepl of nxafj is
  signal yapxkzjor : real;
  signal tdulea : std_logic_vector(3 downto 2);
  signal jqznd : bit;
  signal fqgl : real;
  signal fbhhq : std_logic_vector(3 downto 2);
  signal xdatzlbr : bit;
  signal qfwqdrdi : real;
  signal dbsrfw : std_logic_vector(3 downto 2);
  signal kphaitmv : bit;
  signal pgpjhqc : real;
  signal rabxmakzub : std_logic_vector(3 downto 2);
  signal wwnponxty : bit;
begin
  wzfhbasno : entity work.psdin
    port map (vr => wwnponxty, nkub => rabxmakzub, bpqnkvli => pgpjhqc);
  ahqedwopi : entity work.psdin
    port map (vr => kphaitmv, nkub => dbsrfw, bpqnkvli => qfwqdrdi);
  olonigb : entity work.psdin
    port map (vr => xdatzlbr, nkub => fbhhq, bpqnkvli => fqgl);
  wt : entity work.psdin
    port map (vr => jqznd, nkub => tdulea, bpqnkvli => yapxkzjor);
  
  -- Multi-driven assignments
  tdulea <= "LH";
end irxgossepl;

entity k is
  port (saacafgli : buffer integer_vector(3 downto 0); ktdvn : in time);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture vjqkvo of k is
  signal szboz : std_logic;
  signal cny : real;
  signal z : time;
  signal kuqwvt : std_logic;
begin
  swhpuljb : entity work.nxafj
    port map (xjsytfu => kuqwvt, odzvi => z, c => cny, guixqnng => szboz);
  
  -- Multi-driven assignments
  kuqwvt <= '-';
  kuqwvt <= kuqwvt;
end vjqkvo;

entity tqnk is
  port (gg : linkage character; bqogynr : out bit; shfipuinqi : buffer integer);
end tqnk;

architecture suh of tqnk is
  signal cdy : time;
  signal v : integer_vector(3 downto 0);
begin
  n : entity work.k
    port map (saacafgli => v, ktdvn => cdy);
  
  -- Single-driven assignments
  shfipuinqi <= 2_0_1;
  cdy <= cdy;
end suh;



-- Seed after: 16479295361153147595,11127274767545411571

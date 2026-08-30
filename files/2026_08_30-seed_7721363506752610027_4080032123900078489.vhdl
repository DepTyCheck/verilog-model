-- Seed: 7721363506752610027,4080032123900078489

entity llnubmox is
  port (menxt : out time; bdlikv : buffer boolean_vector(2 downto 4));
end llnubmox;

architecture yvonzxy of llnubmox is
  
begin
  -- Single-driven assignments
  bdlikv <= bdlikv;
  menxt <= 1_3_4.2_2 ms;
end yvonzxy;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (l : buffer time; hwtjtgflqj : linkage std_logic_vector(3 downto 2));
end b;

architecture zc of b is
  signal uptddbkeyh : boolean_vector(2 downto 4);
begin
  ncivzxzbk : entity work.llnubmox
    port map (menxt => l, bdlikv => uptddbkeyh);
end zc;

library ieee;
use ieee.std_logic_1164.all;

entity qx is
  port (w : out std_logic);
end qx;

library ieee;
use ieee.std_logic_1164.all;

architecture troz of qx is
  signal ohwrl : time;
  signal exd : std_logic_vector(3 downto 2);
  signal s : time;
begin
  daonjtfcg : entity work.b
    port map (l => s, hwtjtgflqj => exd);
  ekuczqnu : entity work.b
    port map (l => ohwrl, hwtjtgflqj => exd);
  
  -- Multi-driven assignments
  w <= '0';
end troz;

library ieee;
use ieee.std_logic_1164.all;

entity bxewfskd is
  port (sjtvuuomy : linkage integer; bks : linkage time; hozpfrfsye : inout std_logic_vector(0 to 4); wvzi : out time);
end bxewfskd;

architecture cqbtojwx of bxewfskd is
  signal tbtokooj : boolean_vector(2 downto 4);
  signal smf : boolean_vector(2 downto 4);
  signal hyvnbn : time;
begin
  curwmp : entity work.llnubmox
    port map (menxt => hyvnbn, bdlikv => smf);
  a : entity work.llnubmox
    port map (menxt => wvzi, bdlikv => tbtokooj);
  
  -- Multi-driven assignments
  hozpfrfsye <= hozpfrfsye;
end cqbtojwx;



-- Seed after: 2531675328002439784,4080032123900078489

-- Seed: 3583964222008621815,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity haftpjbwh is
  port (dnh : inout std_logic; knknuyje : linkage boolean_vector(1 to 0); ucuqvorx : linkage integer);
end haftpjbwh;

architecture mbu of haftpjbwh is
  
begin
  -- Multi-driven assignments
  dnh <= '1';
  dnh <= '0';
end mbu;

library ieee;
use ieee.std_logic_1164.all;

entity jjif is
  port (gnnlyi : linkage std_logic; vvohoms : inout std_logic_vector(3 to 3); f : inout real_vector(0 downto 1));
end jjif;

library ieee;
use ieee.std_logic_1164.all;

architecture ozgemsyd of jjif is
  signal eufaow : integer;
  signal l : boolean_vector(1 to 0);
  signal jxhhibb : std_logic;
  signal npgkhdjd : integer;
  signal lfbd : boolean_vector(1 to 0);
  signal gzoup : std_logic;
  signal klussuxzy : integer;
  signal ookutcdl : boolean_vector(1 to 0);
  signal vzx : std_logic;
begin
  clkgx : entity work.haftpjbwh
    port map (dnh => vzx, knknuyje => ookutcdl, ucuqvorx => klussuxzy);
  zgzjdsnakz : entity work.haftpjbwh
    port map (dnh => gzoup, knknuyje => lfbd, ucuqvorx => npgkhdjd);
  obtnpsw : entity work.haftpjbwh
    port map (dnh => jxhhibb, knknuyje => l, ucuqvorx => eufaow);
  
  -- Single-driven assignments
  f <= (others => 0.0);
  
  -- Multi-driven assignments
  vzx <= 'L';
  vvohoms <= (others => 'W');
end ozgemsyd;

library ieee;
use ieee.std_logic_1164.all;

entity nd is
  port (qrzydqnzf : out character; oncyv : buffer std_logic; alzoucz : buffer std_logic_vector(2 downto 0));
end nd;

library ieee;
use ieee.std_logic_1164.all;

architecture tebqaenxp of nd is
  signal s : real_vector(0 downto 1);
  signal ffevrfnm : std_logic_vector(3 to 3);
  signal dvggxbl : integer;
  signal cegphyqy : boolean_vector(1 to 0);
begin
  zhbrzipo : entity work.haftpjbwh
    port map (dnh => oncyv, knknuyje => cegphyqy, ucuqvorx => dvggxbl);
  zzdqcus : entity work.jjif
    port map (gnnlyi => oncyv, vvohoms => ffevrfnm, f => s);
  
  -- Single-driven assignments
  qrzydqnzf <= 'l';
  
  -- Multi-driven assignments
  alzoucz <= "0-0";
  ffevrfnm <= "W";
  oncyv <= 'W';
  oncyv <= '1';
end tebqaenxp;

library ieee;
use ieee.std_logic_1164.all;

entity rxlw is
  port (wk : out std_logic_vector(4 downto 0); zshzuac : linkage character);
end rxlw;

library ieee;
use ieee.std_logic_1164.all;

architecture ebmfonr of rxlw is
  signal a : integer;
  signal hloqrz : boolean_vector(1 to 0);
  signal dhrxxs : integer;
  signal tumrpad : boolean_vector(1 to 0);
  signal ftcedh : std_logic;
begin
  lh : entity work.haftpjbwh
    port map (dnh => ftcedh, knknuyje => tumrpad, ucuqvorx => dhrxxs);
  eaiddfyzfh : entity work.haftpjbwh
    port map (dnh => ftcedh, knknuyje => hloqrz, ucuqvorx => a);
  
  -- Multi-driven assignments
  wk <= "UWULX";
  ftcedh <= 'Z';
  ftcedh <= '-';
  wk <= "ZW010";
end ebmfonr;



-- Seed after: 16993503150617021219,13694093582652240945

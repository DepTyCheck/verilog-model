-- Seed: 11639193696929053751,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity tdoham is
  port (pycdvro : in time; gbuneghcd : linkage std_logic_vector(2 to 2); djivkwwtl : in integer);
end tdoham;

architecture hnczrniy of tdoham is
  
begin
  
end hnczrniy;

library ieee;
use ieee.std_logic_1164.all;

entity tpayugj is
  port (iw : inout std_logic_vector(4 to 3); risizswk : inout time; uuzp : out std_logic_vector(2 to 4));
end tpayugj;

library ieee;
use ieee.std_logic_1164.all;

architecture jygrpi of tpayugj is
  signal tiiflcix : integer;
  signal xyfj : std_logic_vector(2 to 2);
  signal bryyvhglce : integer;
  signal n : integer;
  signal etrxenxjl : std_logic_vector(2 to 2);
begin
  mwvqfhvzux : entity work.tdoham
    port map (pycdvro => risizswk, gbuneghcd => etrxenxjl, djivkwwtl => n);
  vugqxdfwo : entity work.tdoham
    port map (pycdvro => risizswk, gbuneghcd => etrxenxjl, djivkwwtl => bryyvhglce);
  mfpkhs : entity work.tdoham
    port map (pycdvro => risizswk, gbuneghcd => etrxenxjl, djivkwwtl => n);
  eaesy : entity work.tdoham
    port map (pycdvro => risizswk, gbuneghcd => xyfj, djivkwwtl => tiiflcix);
  
  -- Single-driven assignments
  risizswk <= risizswk;
  tiiflcix <= 4_4_1;
  n <= 8#01040#;
  
  -- Multi-driven assignments
  uuzp <= uuzp;
end jygrpi;

entity mmyk is
  port (ysjdokmzhj : out integer);
end mmyk;

library ieee;
use ieee.std_logic_1164.all;

architecture pfvyzy of mmyk is
  signal qugugt : std_logic_vector(2 to 4);
  signal rdreqbt : time;
  signal w : std_logic_vector(4 to 3);
begin
  lvqnljvrxf : entity work.tpayugj
    port map (iw => w, risizswk => rdreqbt, uuzp => qugugt);
  
  -- Single-driven assignments
  ysjdokmzhj <= 4_4_4_4_0;
  
  -- Multi-driven assignments
  w <= w;
  qugugt <= "01L";
  qugugt <= qugugt;
  w <= (others => '0');
end pfvyzy;

entity gp is
  port (nlw : buffer string(5 downto 4));
end gp;

library ieee;
use ieee.std_logic_1164.all;

architecture nnarzqal of gp is
  signal eykr : integer;
  signal jorkysf : std_logic_vector(2 to 2);
  signal urymnv : time;
begin
  bliixv : entity work.tdoham
    port map (pycdvro => urymnv, gbuneghcd => jorkysf, djivkwwtl => eykr);
  io : entity work.mmyk
    port map (ysjdokmzhj => eykr);
  
  -- Single-driven assignments
  nlw <= nlw;
  urymnv <= urymnv;
  
  -- Multi-driven assignments
  jorkysf <= jorkysf;
end nnarzqal;



-- Seed after: 2503471575192843783,2230106469645304029

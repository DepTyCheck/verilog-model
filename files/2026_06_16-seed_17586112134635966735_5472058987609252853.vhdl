-- Seed: 17586112134635966735,5472058987609252853

entity o is
  port (mmmwag : buffer boolean_vector(4 downto 4); ajt : linkage bit; xil : linkage boolean; bv : out time);
end o;

architecture tmimslvqyt of o is
  
begin
  -- Single-driven assignments
  bv <= 0.1_2 ps;
end tmimslvqyt;

entity xx is
  port (ziasxdsl : inout severity_level; yqomaypr : linkage integer);
end xx;

architecture ykjze of xx is
  signal jw : time;
  signal haannqtu : boolean;
  signal huypqxgil : bit;
  signal jq : boolean_vector(4 downto 4);
  signal dlqmyokpwj : time;
  signal viaotqhc : boolean;
  signal cqczo : bit;
  signal hvdsypv : boolean_vector(4 downto 4);
  signal qxqacl : time;
  signal axvutfjgku : boolean;
  signal ekcegvv : bit;
  signal hrakct : boolean_vector(4 downto 4);
  signal fzofzwpreu : time;
  signal caounnsr : boolean;
  signal nu : bit;
  signal mon : boolean_vector(4 downto 4);
begin
  jnwonqib : entity work.o
    port map (mmmwag => mon, ajt => nu, xil => caounnsr, bv => fzofzwpreu);
  ltxmwxryax : entity work.o
    port map (mmmwag => hrakct, ajt => ekcegvv, xil => axvutfjgku, bv => qxqacl);
  xtrvrjvkp : entity work.o
    port map (mmmwag => hvdsypv, ajt => cqczo, xil => viaotqhc, bv => dlqmyokpwj);
  okugoglt : entity work.o
    port map (mmmwag => jq, ajt => huypqxgil, xil => haannqtu, bv => jw);
  
  -- Single-driven assignments
  ziasxdsl <= FAILURE;
end ykjze;

library ieee;
use ieee.std_logic_1164.all;

entity ghuml is
  port (fkna : buffer std_logic_vector(3 downto 2));
end ghuml;

architecture pkyga of ghuml is
  
begin
  -- Multi-driven assignments
  fkna <= ('H', 'L');
  fkna <= ('X', '1');
  fkna <= "X1";
end pkyga;

library ieee;
use ieee.std_logic_1164.all;

entity etxhsclvmw is
  port (lmfprqr : inout std_logic_vector(3 to 1); fuiygqgk : in real; l : inout integer);
end etxhsclvmw;

library ieee;
use ieee.std_logic_1164.all;

architecture k of etxhsclvmw is
  signal sdb : time;
  signal svnjojq : boolean;
  signal mhrum : bit;
  signal imczgc : boolean_vector(4 downto 4);
  signal wxnybvrnm : std_logic_vector(3 downto 2);
begin
  rnkn : entity work.ghuml
    port map (fkna => wxnybvrnm);
  eaoyw : entity work.o
    port map (mmmwag => imczgc, ajt => mhrum, xil => svnjojq, bv => sdb);
  eksp : entity work.ghuml
    port map (fkna => wxnybvrnm);
  
  -- Multi-driven assignments
  lmfprqr <= (others => '0');
  lmfprqr <= "";
  wxnybvrnm <= "1W";
  lmfprqr <= "";
end k;



-- Seed after: 318258004790174780,5472058987609252853

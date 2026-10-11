-- Seed: 7787142286426660058,9099465110516446487

entity omejdulz is
  port (wbocsyvsnt : in bit; d : inout severity_level; zmyyhxvr : inout time);
end omejdulz;

architecture nl of omejdulz is
  
begin
  -- Single-driven assignments
  d <= d;
end nl;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (sggfc : in boolean_vector(0 to 1); enhvanz : out integer; tix : buffer std_logic_vector(2 to 2));
end t;

architecture gxmljyqzv of t is
  
begin
  -- Multi-driven assignments
  tix <= "Z";
  tix <= (others => 'X');
  tix <= "H";
  tix <= (others => 'X');
end gxmljyqzv;

library ieee;
use ieee.std_logic_1164.all;

entity qaj is
  port (m : in std_logic_vector(3 to 4); dvokd : inout character; sffrdk : linkage std_logic; oxlngyrtu : out std_logic_vector(0 downto 0));
end qaj;

architecture endeemd of qaj is
  signal mifenju : time;
  signal sfdyyx : severity_level;
  signal yeepto : bit;
  signal jfjzurdrv : time;
  signal xwdh : severity_level;
  signal dzwvl : bit;
  signal qjvql : time;
  signal sksmntieu : severity_level;
  signal wqt : bit;
  signal anoqjuarns : time;
  signal xhvyjupggl : severity_level;
  signal debw : bit;
begin
  gvg : entity work.omejdulz
    port map (wbocsyvsnt => debw, d => xhvyjupggl, zmyyhxvr => anoqjuarns);
  vwwsqjn : entity work.omejdulz
    port map (wbocsyvsnt => wqt, d => sksmntieu, zmyyhxvr => qjvql);
  exdjw : entity work.omejdulz
    port map (wbocsyvsnt => dzwvl, d => xwdh, zmyyhxvr => jfjzurdrv);
  dqxgeqwgwq : entity work.omejdulz
    port map (wbocsyvsnt => yeepto, d => sfdyyx, zmyyhxvr => mifenju);
  
  -- Single-driven assignments
  wqt <= debw;
  debw <= debw;
  dvokd <= 'v';
  
  -- Multi-driven assignments
  oxlngyrtu <= oxlngyrtu;
  oxlngyrtu <= oxlngyrtu;
end endeemd;



-- Seed after: 6653316287323474243,9099465110516446487

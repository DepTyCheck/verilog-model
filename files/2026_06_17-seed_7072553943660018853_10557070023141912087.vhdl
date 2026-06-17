-- Seed: 7072553943660018853,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity vkuuzsnet is
  port (nrim : buffer time_vector(3 to 0); i : inout time; vycfcrl : linkage integer; rsmsgp : out std_logic_vector(1 downto 2));
end vkuuzsnet;

architecture p of vkuuzsnet is
  
begin
  -- Single-driven assignments
  i <= 8#4545.073# ps;
  nrim <= (others => 0 ns);
  
  -- Multi-driven assignments
  rsmsgp <= (others => '0');
end p;

entity ghornl is
  port (rfpehtpuco : inout time; vs : linkage character);
end ghornl;

library ieee;
use ieee.std_logic_1164.all;

architecture ngqrqspfmn of ghornl is
  signal unrhbou : integer;
  signal gkezrqov : time;
  signal ldoxoadcxk : time_vector(3 to 0);
  signal enykryqoww : std_logic_vector(1 downto 2);
  signal gol : integer;
  signal vjsrtjil : time_vector(3 to 0);
begin
  mhmdxw : entity work.vkuuzsnet
    port map (nrim => vjsrtjil, i => rfpehtpuco, vycfcrl => gol, rsmsgp => enykryqoww);
  vm : entity work.vkuuzsnet
    port map (nrim => ldoxoadcxk, i => gkezrqov, vycfcrl => unrhbou, rsmsgp => enykryqoww);
end ngqrqspfmn;

entity axe is
  port (yi : out severity_level);
end axe;

library ieee;
use ieee.std_logic_1164.all;

architecture iq of axe is
  signal sxkyc : character;
  signal kb : time;
  signal mf : std_logic_vector(1 downto 2);
  signal o : integer;
  signal cjvsfkudzy : time;
  signal z : time_vector(3 to 0);
begin
  k : entity work.vkuuzsnet
    port map (nrim => z, i => cjvsfkudzy, vycfcrl => o, rsmsgp => mf);
  dkbt : entity work.ghornl
    port map (rfpehtpuco => kb, vs => sxkyc);
  
  -- Single-driven assignments
  yi <= ERROR;
  
  -- Multi-driven assignments
  mf <= "";
  mf <= (others => '0');
  mf <= (others => '0');
end iq;

library ieee;
use ieee.std_logic_1164.all;

entity rl is
  port (mzjreb : out integer; mmtc : in std_logic_vector(3 to 0); w : linkage std_logic_vector(3 to 0));
end rl;

architecture kgfbaj of rl is
  signal lnsptgycvc : character;
  signal ugkl : time;
begin
  iacegxex : entity work.ghornl
    port map (rfpehtpuco => ugkl, vs => lnsptgycvc);
  
  -- Single-driven assignments
  mzjreb <= 3;
end kgfbaj;



-- Seed after: 1790176326394292264,10557070023141912087

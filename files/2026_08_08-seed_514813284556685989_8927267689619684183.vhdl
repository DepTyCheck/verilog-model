-- Seed: 514813284556685989,8927267689619684183

entity cgcqx is
  port (kwuv : inout real; ksqjeux : out time_vector(4 to 2); jrmxx : out time);
end cgcqx;

architecture fcgjtct of cgcqx is
  
begin
  -- Single-driven assignments
  jrmxx <= 3 sec;
  kwuv <= 8#7.13#;
  ksqjeux <= (others => 0 ns);
end fcgjtct;

library ieee;
use ieee.std_logic_1164.all;

entity ndufdbatr is
  port (apv : buffer std_logic_vector(0 downto 2); uance : buffer std_logic; irc : linkage real);
end ndufdbatr;

architecture ohdu of ndufdbatr is
  signal mcnzjiw : time;
  signal ymwosc : time_vector(4 to 2);
  signal mz : real;
  signal gnfhdtvje : time;
  signal jgwvjiy : time_vector(4 to 2);
  signal uybpu : real;
  signal xpukflt : time;
  signal w : time_vector(4 to 2);
  signal libn : real;
begin
  twfspacqs : entity work.cgcqx
    port map (kwuv => libn, ksqjeux => w, jrmxx => xpukflt);
  uygsrigu : entity work.cgcqx
    port map (kwuv => uybpu, ksqjeux => jgwvjiy, jrmxx => gnfhdtvje);
  umhy : entity work.cgcqx
    port map (kwuv => mz, ksqjeux => ymwosc, jrmxx => mcnzjiw);
  
  -- Multi-driven assignments
  uance <= 'X';
end ohdu;

entity xhzmavd is
  port (nxl : in integer);
end xhzmavd;

library ieee;
use ieee.std_logic_1164.all;

architecture vyy of xhzmavd is
  signal rzeqi : time;
  signal trpfe : time_vector(4 to 2);
  signal dogcnvr : real;
  signal umkt : real;
  signal s : std_logic;
  signal fhnslxhfo : std_logic_vector(0 downto 2);
  signal uujfuvkt : time;
  signal llrxmyhrpb : time_vector(4 to 2);
  signal icwluq : real;
  signal ypoppqfli : time;
  signal mqc : time_vector(4 to 2);
  signal hsqpgqlju : real;
begin
  lbtwj : entity work.cgcqx
    port map (kwuv => hsqpgqlju, ksqjeux => mqc, jrmxx => ypoppqfli);
  ykwebcfp : entity work.cgcqx
    port map (kwuv => icwluq, ksqjeux => llrxmyhrpb, jrmxx => uujfuvkt);
  kih : entity work.ndufdbatr
    port map (apv => fhnslxhfo, uance => s, irc => umkt);
  pqmatvool : entity work.cgcqx
    port map (kwuv => dogcnvr, ksqjeux => trpfe, jrmxx => rzeqi);
end vyy;



-- Seed after: 9364134709044443162,8927267689619684183

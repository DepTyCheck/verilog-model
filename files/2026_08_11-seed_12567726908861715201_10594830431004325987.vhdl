-- Seed: 12567726908861715201,10594830431004325987

entity rw is
  port (l : linkage time; frbuz : buffer boolean_vector(3 downto 3));
end rw;

architecture xbywva of rw is
  
begin
  -- Single-driven assignments
  frbuz <= (others => TRUE);
end xbywva;

entity fhxjbw is
  port (qyyxxst : in real; sbnopw : out time; jos : linkage real);
end fhxjbw;

architecture oeh of fhxjbw is
  signal uimmlxk : boolean_vector(3 downto 3);
  signal yqnqiac : boolean_vector(3 downto 3);
  signal cf : time;
  signal hlhs : boolean_vector(3 downto 3);
  signal vexjcnzq : time;
  signal ovtasvyg : boolean_vector(3 downto 3);
  signal zvjmdbzn : time;
begin
  nxef : entity work.rw
    port map (l => zvjmdbzn, frbuz => ovtasvyg);
  ynovtdo : entity work.rw
    port map (l => vexjcnzq, frbuz => hlhs);
  p : entity work.rw
    port map (l => cf, frbuz => yqnqiac);
  d : entity work.rw
    port map (l => sbnopw, frbuz => uimmlxk);
end oeh;

library ieee;
use ieee.std_logic_1164.all;

entity ob is
  port (galjfufnt : out std_logic; d : in std_logic_vector(2 to 4); bpkotjnxr : inout time; dcmnw : linkage std_logic);
end ob;

architecture kdhk of ob is
  
begin
  -- Single-driven assignments
  bpkotjnxr <= bpkotjnxr;
  
  -- Multi-driven assignments
  galjfufnt <= galjfufnt;
  galjfufnt <= 'U';
  galjfufnt <= 'H';
end kdhk;

entity lgjqrqki is
  port (pcfmjjmfos : linkage real);
end lgjqrqki;

library ieee;
use ieee.std_logic_1164.all;

architecture ws of lgjqrqki is
  signal tudgtfuqlr : boolean_vector(3 downto 3);
  signal avktwmkcvz : time;
  signal tjlptikrn : boolean_vector(3 downto 3);
  signal qnygx : time;
  signal jkhokycxzi : time;
  signal capppsep : std_logic_vector(2 to 4);
  signal nkqnlgl : std_logic;
begin
  iulcklba : entity work.ob
    port map (galjfufnt => nkqnlgl, d => capppsep, bpkotjnxr => jkhokycxzi, dcmnw => nkqnlgl);
  y : entity work.rw
    port map (l => qnygx, frbuz => tjlptikrn);
  lcyptte : entity work.rw
    port map (l => avktwmkcvz, frbuz => tudgtfuqlr);
  
  -- Multi-driven assignments
  nkqnlgl <= '1';
  nkqnlgl <= '-';
  nkqnlgl <= '-';
end ws;



-- Seed after: 10429654905697030338,10594830431004325987

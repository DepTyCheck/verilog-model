-- Seed: 9662289406466077266,10463297573877745897

entity pcqyrdvdi is
  port (ob : buffer integer);
end pcqyrdvdi;

architecture sgvp of pcqyrdvdi is
  
begin
  -- Single-driven assignments
  ob <= ob;
end sgvp;

library ieee;
use ieee.std_logic_1164.all;

entity aay is
  port (ovruuphtdu : out time; ddseiz : inout std_logic_vector(3 downto 1));
end aay;

architecture by of aay is
  signal cllipcqhe : integer;
  signal me : integer;
  signal eu : integer;
begin
  pmwumuhf : entity work.pcqyrdvdi
    port map (ob => eu);
  bzrr : entity work.pcqyrdvdi
    port map (ob => me);
  txyc : entity work.pcqyrdvdi
    port map (ob => cllipcqhe);
  
  -- Single-driven assignments
  ovruuphtdu <= 42 ps;
  
  -- Multi-driven assignments
  ddseiz <= ('W', '-', 'W');
  ddseiz <= ddseiz;
  ddseiz <= ddseiz;
  ddseiz <= ddseiz;
end by;

entity nwjqjj is
  port (tzxpwcjln : inout character; ldxuihoth : buffer severity_level);
end nwjqjj;

library ieee;
use ieee.std_logic_1164.all;

architecture c of nwjqjj is
  signal bo : std_logic_vector(3 downto 1);
  signal qtnaimof : time;
  signal bnmlbyqa : integer;
begin
  eflyxoqf : entity work.pcqyrdvdi
    port map (ob => bnmlbyqa);
  gjhpkwa : entity work.aay
    port map (ovruuphtdu => qtnaimof, ddseiz => bo);
  
  -- Single-driven assignments
  ldxuihoth <= ERROR;
  tzxpwcjln <= 'y';
  
  -- Multi-driven assignments
  bo <= bo;
end c;



-- Seed after: 3964992566233275486,10463297573877745897

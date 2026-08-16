-- Seed: 9087261872618084309,13857275728440271305

entity tsemmw is
  port (xq : inout time; giyjlijv : inout real);
end tsemmw;

architecture eoojssxv of tsemmw is
  
begin
  
end eoojssxv;

entity imosmkzxj is
  port (zpbck : buffer integer);
end imosmkzxj;

architecture bwjswpozl of imosmkzxj is
  signal rbavy : real;
  signal tnqftzgjsb : time;
begin
  axitnsu : entity work.tsemmw
    port map (xq => tnqftzgjsb, giyjlijv => rbavy);
  
  -- Single-driven assignments
  zpbck <= 1040;
end bwjswpozl;

entity agynj is
  port (atgldswr : in integer_vector(0 downto 1); bjimygvej : in boolean_vector(0 to 3); st : out time);
end agynj;

architecture cktfb of agynj is
  signal a : real;
  signal ccxmsfizwf : time;
  signal uvyyisazty : integer;
  signal ulggro : real;
  signal vucbb : time;
  signal jcznn : real;
begin
  nv : entity work.tsemmw
    port map (xq => st, giyjlijv => jcznn);
  scvdnhxrv : entity work.tsemmw
    port map (xq => vucbb, giyjlijv => ulggro);
  xezhi : entity work.imosmkzxj
    port map (zpbck => uvyyisazty);
  t : entity work.tsemmw
    port map (xq => ccxmsfizwf, giyjlijv => a);
end cktfb;

entity y is
  port (hkunnw : in real);
end y;

architecture kwnp of y is
  signal wr : real;
  signal acs : time;
begin
  f : entity work.tsemmw
    port map (xq => acs, giyjlijv => wr);
end kwnp;



-- Seed after: 1929957581658893743,13857275728440271305

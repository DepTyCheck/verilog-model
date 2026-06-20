-- Seed: 16691445962332142436,3924983747739634027

entity pjkqx is
  port (lobda : linkage severity_level; ds : buffer integer);
end pjkqx;

architecture zwjrg of pjkqx is
  
begin
  -- Single-driven assignments
  ds <= 2#000#;
end zwjrg;

entity ixtgtfnfq is
  port (khdho : buffer real; jxykzieeq : out time; ouca : inout time_vector(2 to 3); etrvqyurtb : inout boolean_vector(2 to 1));
end ixtgtfnfq;

architecture m of ixtgtfnfq is
  signal rtnrmc : integer;
  signal h : severity_level;
  signal nbqlir : integer;
  signal li : severity_level;
begin
  fpli : entity work.pjkqx
    port map (lobda => li, ds => nbqlir);
  jmdmp : entity work.pjkqx
    port map (lobda => h, ds => rtnrmc);
  
  -- Single-driven assignments
  etrvqyurtb <= (others => TRUE);
  ouca <= (8#577# us, 4 min);
  jxykzieeq <= 2#1.11# ms;
end m;

entity vaz is
  port (et : inout integer; lrnzsje : inout integer_vector(1 to 3); nydvjz : in string(5 downto 4));
end vaz;

architecture u of vaz is
  signal zkaijgrh : severity_level;
  signal yquz : boolean_vector(2 to 1);
  signal kp : time_vector(2 to 3);
  signal r : time;
  signal funfvlfa : real;
  signal ktwa : integer;
  signal fq : severity_level;
  signal wfoicalmv : integer;
  signal w : severity_level;
begin
  hminhrjqjq : entity work.pjkqx
    port map (lobda => w, ds => wfoicalmv);
  kgkb : entity work.pjkqx
    port map (lobda => fq, ds => ktwa);
  ofphns : entity work.ixtgtfnfq
    port map (khdho => funfvlfa, jxykzieeq => r, ouca => kp, etrvqyurtb => yquz);
  qdwbyugn : entity work.pjkqx
    port map (lobda => zkaijgrh, ds => et);
end u;



-- Seed after: 14593874346901664135,3924983747739634027

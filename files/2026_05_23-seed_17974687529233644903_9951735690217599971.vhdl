-- Seed: 17974687529233644903,9951735690217599971



entity yrtpw is
  port (ujrdrsei : in integer; mnpu : buffer real; ymvyq : out time; ubvazvbom : buffer time);
end yrtpw;



architecture gbhvzwvh of yrtpw is
  
begin
  
end gbhvzwvh;



entity jhpd is
  port (xjqzwcqi : inout real_vector(2 downto 1); cztshm : out time_vector(3 downto 1); fw : in time; ojmf : buffer integer);
end jhpd;



architecture axp of jhpd is
  signal iuxy : time;
  signal ijgzqv : time;
  signal nuokl : real;
  signal wtxaywrtxq : time;
  signal mldyedrzaf : time;
  signal r : real;
  signal ifjckr : integer;
  signal npflcwd : time;
  signal prefsk : time;
  signal ays : real;
begin
  xydmgxq : entity work.yrtpw
    port map (ujrdrsei => ojmf, mnpu => ays, ymvyq => prefsk, ubvazvbom => npflcwd);
  eplvw : entity work.yrtpw
    port map (ujrdrsei => ifjckr, mnpu => r, ymvyq => mldyedrzaf, ubvazvbom => wtxaywrtxq);
  atke : entity work.yrtpw
    port map (ujrdrsei => ojmf, mnpu => nuokl, ymvyq => ijgzqv, ubvazvbom => iuxy);
end axp;



-- Seed after: 14626921753860313210,9951735690217599971

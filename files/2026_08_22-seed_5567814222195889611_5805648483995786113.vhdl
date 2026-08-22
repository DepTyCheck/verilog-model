-- Seed: 5567814222195889611,5805648483995786113

entity ode is
  port (osoukgysbe : linkage real; ugujz : buffer severity_level; hsq : out time; rv : linkage severity_level);
end ode;

architecture dq of ode is
  
begin
  -- Single-driven assignments
  hsq <= 2 min;
  ugujz <= NOTE;
end dq;

entity jvjyq is
  port (hmkkqud : in severity_level; tqihqo : inout integer; m : out real; qxrah : linkage real);
end jvjyq;

architecture wqnatlh of jvjyq is
  signal e : severity_level;
  signal evh : time;
  signal xyxve : severity_level;
  signal z : severity_level;
  signal ivumcjc : time;
  signal rb : severity_level;
  signal ccg : real;
  signal axzucspmrs : severity_level;
  signal ubzbxoila : time;
  signal jks : severity_level;
  signal se : real;
  signal vgo : severity_level;
  signal ldpvpfjufr : time;
  signal ryubitgdol : severity_level;
  signal fk : real;
begin
  olqf : entity work.ode
    port map (osoukgysbe => fk, ugujz => ryubitgdol, hsq => ldpvpfjufr, rv => vgo);
  mhf : entity work.ode
    port map (osoukgysbe => se, ugujz => jks, hsq => ubzbxoila, rv => axzucspmrs);
  tuv : entity work.ode
    port map (osoukgysbe => ccg, ugujz => rb, hsq => ivumcjc, rv => z);
  zlk : entity work.ode
    port map (osoukgysbe => qxrah, ugujz => xyxve, hsq => evh, rv => e);
  
  -- Single-driven assignments
  m <= m;
  tqihqo <= 1_4;
end wqnatlh;



-- Seed after: 3741388556980248582,5805648483995786113

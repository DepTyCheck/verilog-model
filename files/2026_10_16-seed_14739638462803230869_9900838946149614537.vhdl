-- Seed: 14739638462803230869,9900838946149614537

entity nlhr is
  port (jdthoa : buffer time_vector(4 downto 0); tuyd : linkage severity_level; t : buffer time; ovguypkzna : linkage integer);
end nlhr;

architecture wn of nlhr is
  
begin
  -- Single-driven assignments
  t <= 0 hr;
end wn;

entity zn is
  port (nscejn : buffer time);
end zn;

architecture fmox of zn is
  signal wcs : integer;
  signal ki : time;
  signal kmwfrcg : severity_level;
  signal nvczwmfdx : time_vector(4 downto 0);
  signal k : integer;
  signal wuhtu : time;
  signal lpfj : severity_level;
  signal adpuicoi : time_vector(4 downto 0);
  signal tasm : integer;
  signal yvoomb : time;
  signal jprnfgbw : severity_level;
  signal krhuuomsr : time_vector(4 downto 0);
begin
  v : entity work.nlhr
    port map (jdthoa => krhuuomsr, tuyd => jprnfgbw, t => yvoomb, ovguypkzna => tasm);
  nmqnxr : entity work.nlhr
    port map (jdthoa => adpuicoi, tuyd => lpfj, t => wuhtu, ovguypkzna => k);
  trffc : entity work.nlhr
    port map (jdthoa => nvczwmfdx, tuyd => kmwfrcg, t => ki, ovguypkzna => wcs);
  
  -- Single-driven assignments
  nscejn <= 3.3_2_0 us;
end fmox;



-- Seed after: 11234918966911835289,9900838946149614537

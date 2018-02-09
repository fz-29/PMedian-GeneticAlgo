#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>

#define MAX_CUSTOMERS 100
#define MAX_FACILITY 100
#define CHROMOSOME_SIZE 100
#define MUTATION_RATE 0.3
#define POPULATION_SIZE 5
#define MAX_GENERATIONS 50
#define SELECTION_PROBABILITY 0.2

using namespace std;

int count_customers, count_total_facilities, count_planned_facilities;

struct Individual
{
	// the number of "1" in the chromosome must be equal to the facilities targeted
	short int chromosome[CHROMOSOME_SIZE];
	int fitness_value;
};

struct Population
{
	int size;
	Individual solutions[POPULATION_SIZE];
};

// distance of customer(row)-facility(column)
int distances[MAX_CUSTOMERS][MAX_FACILITY] = {0};

// demand of customer
int demand[MAX_CUSTOMERS];

// UTILITY FUNCTIONS
int find_nearest_facility(int customer, Individual individual)
{
	// find the minimum distance from the selected facility according to the 'individual'
	vector< pair <int, int> > sortedDistances;
	int f;
	for (f = 0; f < count_total_facilities; f++)
	{
		sortedDistances.push_back(make_pair(distances[customer][f], f));
	}
	sort(sortedDistances.begin(), sortedDistances.end());
	for (f = 0; f < count_total_facilities; f++)
	{
		int facility = sortedDistances[f].second;
		if (individual.chromosome[facility] == 1)
		{
			return facility;
		}
	}
	return f-1;
}

int fitness_function(Individual individual)
{
	int fitness_value = 0;
	for (int c = 0; c < count_customers; c++)
	{
		int facility = find_nearest_facility(c, individual);
		fitness_value += distances[c][facility] * demand[c];
	}
	return fitness_value;
}

int count_ones(const short int c[])
{
	int i, ones;
	for(i = 0, ones = 0; i < count_total_facilities; i++)
	{
		if (c[i] == 1) ones++;
	}
	return ones;
}

void maintain_ones_in_chromosome(short int c[])
{
	// out of count_total_facilities bits, count_planned_facilities should be always set for proper solution.
	while(count_ones(c) != count_planned_facilities)
	{
		int r = rand() % count_total_facilities;
		if (count_ones(c) > count_planned_facilities)
		{
			if (c[r] == 1)
			{
				c[r] = 0;
			}
		}
		else
		{
			if (c[r] == 0)
			{
				c[r] = 1;
			}
		}
	}
}

void printChromosome(short int c[])
{
	for(int i = 0; i < count_total_facilities; i++)
	{
		cout<<c[i]<<" ";
	}
	cout<<"\n";
}

Individual generateIndividual()
{
	Individual ind;
	for (int i = 0; i < count_total_facilities; i++)
	{
		ind.chromosome[i] = (rand() * clock()) % 2; // for more randomness
	}
	maintain_ones_in_chromosome(ind.chromosome);
	ind.fitness_value = fitness_function(ind);
	return ind;
}

Individual update_best_solution(Individual best, Population P)
{
	for (int i = 0; i < POPULATION_SIZE; i++)
	{
		if(P.solutions[i].fitness_value < best.fitness_value)
		{
			best = P.solutions[i];
		}
	}
	return best;
}

pair<int, int> select_parents(Population P)
{
	int p1, p2;
	p1 = rand() % POPULATION_SIZE;
	do
	{
		p2 = rand() % POPULATION_SIZE;
	} while(p1 == p2);
	return make_pair(p1, p2);
};

// FUNCTIONS FOR GENETIC OPERATORS
void perform_crossover(short int chromosome1[], short int chromosome2[], short int c3[])
{
	// length of chromosome is equal to the total number of the facilities
	int partition;
	// ensure participation of chromosomes of both the parents.
	partition = count_total_facilities * 0.2 + (rand() % count_total_facilities) * 0.7;

	// randomly swap the chromosome for more stochasticity
	short int *c1, *c2;
	c1 = chromosome1;
	c2 = chromosome2;
	if (rand() % 100 < 50) // 0.5 probability
	{
		c2 = chromosome1;
		c1 = chromosome2;
	}
	int i;
	for (i = 0; i <= partition && i < count_total_facilities; i++)
	{
		c3[i] = c1[i];
	}
	for (; i < count_total_facilities; i++)
	{
		c3[i] = c2[i];
	}
	maintain_ones_in_chromosome(c3);
}

void perform_mutation(short int c[])
{
	// Only swap allowed between 0 and 1 to keep sum same, PROB_SWAP
	if (rand() % 100 < MUTATION_RATE * 100)
	{
		int r1, r2;
		r1 = rand() % count_total_facilities;
		r2 = rand() % count_total_facilities;
		while (r1 == r2)
		{
			r2 = rand() % count_total_facilities;
		}
		short int t;
		t = c[r1];
		c[r1] = c[r2];
		c[r2] = t;
	}
}

void perform_selection(Population &P, Individual offspring, pair<int, int> parents, int generation)
{
	if (P.solutions[parents.first].fitness_value > offspring.fitness_value)
	{
		P.solutions[parents.first] = offspring;
	}
	else if (P.solutions[parents.second].fitness_value > offspring.fitness_value)
	{
		P.solutions[parents.second] = offspring;
	}
	else
	{
		// some chances for survival of a less fit offspring
		// the chance decreases with as generation progresses.
		if (rand() % 1000 < SELECTION_PROBABILITY * 1000.0 / generation)
		{
			if(rand() % 100 < 50)
			{
				P.solutions[parents.first] = offspring;
			}
			else
			{
				P.solutions[parents.second] = offspring;
			}
//            cout<<"\nLess fit offspring selected in generation "<<generation<<endl;
		}
	}
}

// GENETIC EVOLUTION
Individual genetic_evolution()
{
	//initialize population
	Population P;
	Individual bestIndividual;
	for (int i = 0; i < POPULATION_SIZE; i++)
	{
		srand ( i * time(NULL) * clock() );
		P.solutions[i] = generateIndividual();
		P.solutions[i].fitness_value = fitness_function(P.solutions[i]);
	}
	P.size = POPULATION_SIZE;
	bestIndividual = P.solutions[0];
	bestIndividual = update_best_solution(bestIndividual, P);

	for (int g = 0; g < MAX_GENERATIONS; g++)
	{
		Population Pnext = P;
		srand ( g * time(NULL) * clock() );
		for (int i = 0; i < POPULATION_SIZE; i++)
		{
			// SELECT 2 PARENTS
			pair<int, int> parents = select_parents(P);
			// Crossover to generate new off-spring
			Individual offspring;
			perform_crossover(P.solutions[parents.first].chromosome, P.solutions[parents.second].chromosome, offspring.chromosome);
			// Mutate
			perform_mutation(offspring.chromosome);
			offspring.fitness_value = fitness_function(offspring);
			// Perform selection
			perform_selection(Pnext, offspring, parents, g);
		}
		bestIndividual = update_best_solution(bestIndividual, Pnext);
		cout<<"\n Best Individual Score : "<< bestIndividual.fitness_value<<endl;
		printChromosome(bestIndividual.chromosome);
		P = Pnext;
	}
	return bestIndividual;
}

void printPMedianSolution(Individual individual)
{
	cout<<"\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
	cout<<"\nCost : "<<individual.fitness_value;
	cout<<"\nChromosome: "; printChromosome(individual.chromosome);
	cout<<"\nThe following facilities must be finally selected : ";
	for (int f = 0; f < count_total_facilities; f++)
	{
		if (individual.chromosome[f] == 1)
		{
			cout<<"\n\tFacility "<<f+1;
		}
	}
	cout<<"\nCustomers to be served by following facilities : ";
	for (int c = 0; c < count_customers; c++)
	{
		cout<<"\n\tCustomer "<<c+1<<" : "<<find_nearest_facility(c, individual) + 1;
	}
}

int main()
{
	int t;
	cout<<"\nSolving P-Model using Genetic Algorithm"<<endl;
	cout<<"========================================"<<endl;
	cout<<"\nP-Model Problem Specifications"<<endl;
	cout<<"\nEnter total number of facilities under consideration : ";
	cin>>count_total_facilities;
	cout<<"\nEnter total number of facilites to be constructed";
	cin>>count_planned_facilities;
	cout<<"\nEnter the number of customers";
	cin>>count_customers;
	cout<<"\nEnter the demands of the customers";
	for (int i = 0; i < count_customers; i++)
	{
		cout<<"\nEnter the demand of customer "<<i+1<<" :";
		cin>>t;
		demand[i] = t;
	}
	cout<<"\nEnter the distance matrix:";
	for( int i = 0; i < count_customers; i++)
	{
		cout<<"\nCustomer "<<i+1<<" :\n";
		for(int j = 0; j < count_total_facilities; j++)
		{
			cout<<"\n\t\tFacility "<<j+1<<" :";
			cin>>t;
			distances[i][j] = t;
		}
	}
	cout<<"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"<<endl;
	cout<<"Genetic Algorithm Specifications";

	cout<<"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"<<endl;
	cout<<"Runs of Genetic Algorithm to solve P-Model problem"<<endl;
	Individual best = genetic_evolution();
	printPMedianSolution(best);
	return 0;
}